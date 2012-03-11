/*****************************************************************

  libjpeg wrapper.

  Copyright (c) 2008-2012, Andy Hefner <ahefner@gmail.com>

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation
  files (the "Software"), to deal in the Software without
  restriction, including without limitation the rights to use,
  copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following
  conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE.

 *****************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <jpeglib.h>
#include <assert.h>
#include <setjmp.h>
#include <sys/stat.h>

enum decoding_state { init=0, error, warning, working, finished };

struct jpeg_decoder_state
{
    /* Public state */
    enum decoding_state state;
    unsigned input_width, input_height;
    unsigned output_width, output_height;
    char buffer[JMSG_LENGTH_MAX];
    /* Internal state */
    FILE *input;
    jmp_buf jpeg_error_trampoline;
    int cinfo_initialized;
    struct jpeg_decompress_struct cinfo;
    struct jpeg_error_mgr errmgr;
    JSAMPROW *rows;
    JSAMPLE *samples;
    int message_pending;
};

static void handle_error_exit (struct jpeg_common_struct *ptr)
{
    struct jpeg_decoder_state *st = ptr->client_data;
    longjmp(st->jpeg_error_trampoline, 1);
}

static void handle_message_output (struct jpeg_common_struct *ptr)
{
    struct jpeg_decoder_state *st = ptr->client_data;
    ptr->err->format_message(ptr, st->buffer);
    st->message_pending = 1;
    st->state = warning;
}

struct jpeg_decoder_state* jlo_new_decoder (void)
{
    struct jpeg_decoder_state *st = malloc(sizeof(*st));
    if (st == NULL) return NULL;
    bzero(st, sizeof(*st));
    st->samples = 0;
    st->rows = 0;
    st->cinfo.client_data = st;
    st->state = init;
    sprintf(st->buffer, "Uninitialized");
    return st;
}

void jlo_free_decoder (struct jpeg_decoder_state *st)
{
    if (st != NULL)
    {
        if (st->input) fclose(st->input);
        if (st->rows) free(st->rows);
        if (st->samples) free(st->samples);
        if (st->cinfo_initialized)
            jpeg_destroy_decompress(&st->cinfo);

        bzero(st, sizeof(*st));
        free(st);
    }
}

void jlo_mode_fastest (struct jpeg_decoder_state *st)
{
    st->cinfo.dct_method = JDCT_FASTEST;   /* Tiny speedup. */
    st->cinfo.do_fancy_upsampling = FALSE; /* Considerable speedup. */
    st->cinfo.do_block_smoothing = FALSE;  /* Irrelevant. */
}

void jlo_mode_high_quality (struct jpeg_decoder_state *st)
{
    st->cinfo.dct_method = JDCT_FLOAT;
    st->cinfo.do_fancy_upsampling = TRUE;
    st->cinfo.do_block_smoothing = TRUE;
}

void jlo_set_downscale (struct jpeg_decoder_state *st, int denom)
{
    /* For thumbnailing, it makes a huge difference to let the JPEG
       library do this the downscaling, because it can fold it into
       the IDCT by discarding smaller coefficients (or so I guess). */
    st->cinfo.scale_num = 1;
    switch (denom)
    {
    case 1:
        st->cinfo.scale_denom = 1;
        break;
    case 2:
        st->cinfo.scale_denom = 2;
        break;
    case 4:
        st->cinfo.scale_denom = 4;
        break;
    case 8:
        st->cinfo.scale_denom = 8;
        break;
    default:
        fprintf(stderr, "jlo_set_downscale: illegal denominator %i\n", denom);
        __builtin_abort();
        break;
    }
}

int jlo_init_from_filename (struct jpeg_decoder_state *st, char *filename)
{
    if (st == NULL) return 0;
    if (st->state != init) return 0;

    st->state = working;
    sprintf(st->buffer, "Ready.");

    st->input = fopen(filename, "rb");
    if (!st->input) {
        sprintf(st->buffer, "Unable to open input file.");
        st->state = error;
        return 1;
    }
    st->message_pending = 0;

    jpeg_create_decompress(&st->cinfo);
    st->cinfo_initialized = 1;
    if (setjmp(st->jpeg_error_trampoline)) goto fatal_with_message;

    st->cinfo.err = jpeg_std_error(&st->errmgr);
    st->errmgr.error_exit = handle_error_exit;
    st->errmgr.output_message = handle_message_output;

    st->cinfo.progress = NULL;
    jpeg_stdio_src(&st->cinfo, st->input);
    jpeg_read_header(&st->cinfo, TRUE);
    st->input_width = st->cinfo.image_width;
    st->input_height = st->cinfo.image_height;

    jlo_mode_fastest(st);
    jlo_set_downscale(st,1);

success:
    return;

fatal_with_message:
    st->errmgr.output_message((struct jpeg_common_struct *)&st->cinfo);
fatal:
    st->state = error;
    return;
}

void jlo_start_decompress (struct jpeg_decoder_state *st)
{
    int i;
    if (st->state == error) return;

    if (setjmp(st->jpeg_error_trampoline)) goto fatal_with_message;

    jpeg_start_decompress(&st->cinfo);

    st->output_width = st->cinfo.output_width;
    st->output_height = st->cinfo.output_height;

    st->samples = calloc(st->cinfo.output_width *
                         st->cinfo.output_height,
                         st->cinfo.num_components);
    st->rows = malloc(sizeof(JSAMPROW) * st->cinfo.output_height);

    for (i=0; i<st->cinfo.output_height; i++) {
        st->rows[i] = st->samples + i * st->cinfo.output_width * st->cinfo.num_components;
    }

    if (!st->samples || !st->rows) {
        sprintf(st->buffer, "Unable to allocate memory for image.");
        goto fatal;
    }

success:
    return;

fatal_with_message:
    st->errmgr.output_message((struct jpeg_common_struct *)&st->cinfo);
fatal:
    st->state = error;
    return;
}

void jlo_work (struct jpeg_decoder_state *st, unsigned maxrows)
{
    int i;
    if (st->state == error || st->state == finished) return;
    st->state = working;
    st->message_pending = 0;

    if (setjmp(st->jpeg_error_trampoline))
    {
        /* Decode error. */
        st->state = error;
        fprintf(stderr, "\nWHAM!\n");
        return;
    }

    /* Perhaps it's possible that multiple messages can arrive while
       we're inside this loop, in which case we lose all but the
       last. Oh well.  It isn't worth any effort to handle this. */
    i = 0;
    while (!st->message_pending &&
           (i++ < maxrows) &&
           (st->cinfo.output_scanline < st->cinfo.output_height)) {
        unsigned num_to_read = st->cinfo.output_height - st->cinfo.output_scanline;
        /*
        printf("  read loop: %i of %i, reading %i lines\n",
               st->cinfo.output_scanline,
               st->cinfo.output_height,
            num_to_read);
        */

        if (num_to_read > maxrows) num_to_read = maxrows;
        jpeg_read_scanlines(&st->cinfo,
                            st->rows + st->cinfo.output_scanline,
                            num_to_read);
    }

    if ((i >= maxrows) || (st->message_pending)) return;

    /* Finished decoding. */
    jpeg_finish_decompress(&st->cinfo);
    st->state = finished;
    sprintf(st->buffer, "Decoding complete.");
    return;
}

void jlo_copy_output (struct jpeg_decoder_state *st, uint32_t *output)
{
    JSAMPLE *in = st->samples;
    int i, n;
    n = st->output_width * st->output_height;
    assert(sizeof(JSAMPLE) == 1);

    switch (st->cinfo.num_components) {
    case 1:                     /* Grayscale image. */
        /* Someday it'd be nice to return these to Lisp as 8bpp images,
           but for now I'll convert them up to RGB.*/
        for (i=0; i<n; i++) {
            uint32_t tmp = in[i];
            output[i] = 0xFF000000 | tmp | (tmp<<8) | (tmp<<16);
        }
        break;
    case 3:
        for (i=0; i<n; i++) {
            output[i] = 0xFF000000 | in[i*3] | (in[i*3+1]<<8) | (in[i*3+2]<<16);
        }
        break;
    default:
        fprintf(stderr, "cinfo.num_components=%i -- What now?", st->cinfo.num_components);
        break;
    }
}

#ifdef BUILD_TEST_PROGRAM

/***  Test program  ***************************************************/

void load_jpeg_test (char *filename)
{
    struct jpeg_decoder_state *st;

    st = jlo_new_decoder();
    jlo_init_from_filename(st, filename);
//    jlo_set_downscale(st,8);
//    jlo_mode_high_quality(st);
    jlo_start_decompress(st);

    if (st->state == working)
    {
        printf("%4i x %4i x %i ", st->cinfo.output_width, st->cinfo.output_height, st->cinfo.num_components);
        fflush(stdout);
    }

    for (;;)
    {
        switch (st->state) {
        case error:
            printf("\n    Error: %s\n", st->buffer);
            jlo_free_decoder(st);
            return;
        case warning:
            printf("\n    Warning: %s\n", st->buffer);
            break;
        case working:
            putchar('.');
            break;
        case finished:
            puts("*\n");
            jlo_free_decoder(st);
            return;
        }

        fflush(stdout);
        jlo_work(st, 100);
    }
}

int main (int argc, char **argv)
{
    int i = 1;
    long long total_size = 0;
    int decode_only_mode = 0;

    if ((argc >= 2) && (!strcmp(argv[1], "--no-decode"))) decode_only_mode = 1;

    for (; i<argc; i++) {
        struct stat buf;
        if (!lstat(argv[i], &buf)) {
            total_size += buf.st_size;
            printf("(%4i /%4i) %70s %10lli on disk, ",
                   i, argc-1,
                   argv[i], (int) buf.st_size);
            fflush(stdout);
            if (!decode_only_mode) load_jpeg_test(argv[i]);
            else {
                void *data = malloc(buf.st_size);
                assert(data);
                FILE *in = fopen(argv[i], "rb");
                if (in) {
                    if (fread(data, buf.st_size, 1, in) != 1)
                        printf("Failed to read whole file.\n");
                    else printf("Read %i bytes.\n", (int)buf.st_size);
                    fclose(in);
                    free(data);
                } else printf("Skipping %s\n", argv[i]);
            }
        } else perror("lstat");
    }
    printf("\nTotal input size: %lli MB.\n", total_size / 1024ll / 1024ll);

    return 0;
}

#endif
