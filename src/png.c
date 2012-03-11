/*****************************************************************

  libpng wrapper.

  Copyright (c) 2012, Andy Hefner <ahefner@gmail.com>

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
#include <stdint.h>
#include <string.h>
#include <assert.h>

#include <png.h>

/* Not using this. The error handler always cleans up before returning
   control to the caller. */
/* enum png_reader_state { error = 0, finished }; */

struct png_reader
{
/*    enum png_reader_state state;*/
    unsigned width, height;
    unsigned channels;
    png_structp png;
    png_infop info;
};


void
free_png_reader (struct png_reader *pr)
{
    if (pr)
    {
        /* I doubt libpng would dare throw an error while I'm
         * destroying its structures, but I'd rather we didn't crash
         * in the unlikely event that this may occur. */
        if (setjmp(png_jmpbuf(pr->png))) {
            free(pr);
            return;
        }

        if (pr->png && pr->info)
        {
            png_destroy_read_struct(&pr->png, &pr->info, NULL);
        }
        else if (pr->png)
        {
            /* Is this really necessary? I'm just aping the examples. */
            png_destroy_read_struct(&pr->png, NULL, NULL);
        }

        bzero(pr, sizeof(*pr));
        free(pr);
    }
}

static void
my_png_error_fn (png_structp png, const char *msg)
{
    fprintf(stderr, "* Error: %s\n", msg);
    longjmp(png_jmpbuf(png), 1);
}

static void
my_png_warning_fn (png_structp png, const char *msg)
{
    fprintf(stderr, "* Warning: %s\n", msg);
}

struct png_reader *
open_and_decode_png_file (const char *filename)
{
    FILE *in;
    unsigned char header[8];
    struct png_reader *pr = NULL;
    png_uint_32 width, height;
    int interlace, compression, filter, depth, color_type;

    in = fopen(filename, "rb");

    if (!in) return NULL;

    fread(header, 1, sizeof(header), in);
    if (png_sig_cmp(header, 0, sizeof(header))) {
        fclose(in);
        return;
    }

    pr = calloc(1, sizeof(*pr));
    if (!pr) return NULL;

    pr->png = png_create_read_struct(PNG_LIBPNG_VER_STRING,
                                     (png_voidp)pr,
                                     my_png_error_fn,
                                     my_png_warning_fn);

    if (!pr->png) {
        free(pr);
        return NULL;
    }

    pr->info = png_create_info_struct(pr->png);
    if (!pr->info) {
        png_destroy_read_struct(&pr->png, NULL, NULL);
        free(pr);
        return NULL;
    }

    /* Error handler: */
    if (setjmp(png_jmpbuf(pr->png))) {
        png_destroy_read_struct(&pr->png, &pr->info, NULL);
        fclose(in);
        free(pr);
        return NULL;
    }

    png_init_io(pr->png, in);
    png_set_sig_bytes(pr->png, sizeof(header));

    png_read_png(pr->png,
                 pr->info,
                 PNG_TRANSFORM_STRIP_16 |
                 PNG_TRANSFORM_GRAY_TO_RGB |
                 PNG_TRANSFORM_EXPAND |
                 PNG_TRANSFORM_PACKING |
                 PNG_TRANSFORM_PACKSWAP,
                 NULL);

    png_get_IHDR(pr->png, pr->info,
                 &width,
                 &height,
                 &depth,
                 &color_type,
                 &interlace,
                 &compression,
                 &filter);

    pr->width  = width;
    pr->height = height;

    pr->channels = png_get_channels(pr->png, pr->info);

    fclose(in);

    return pr;
}

void png_copy_output (struct png_reader *pr, uint32_t *output)
{
    unsigned w = pr->width, h = pr->height, channels = pr->channels;
    int i, j;
    png_bytepp rows = png_get_rows(pr->png, pr->info);

    assert(png_get_rowbytes(pr->png,pr->info) == (channels * w));

    switch (pr->channels)
    {

    case 3:
    {
        for (i=0; i<h; i++) {
            png_bytep row = rows[i];
            uint32_t *outptr = output + i*w;

            for (j=0; j<w; j++) {

                outptr[j] =
                    0xFF000000
                    | row[j*3]
                    | (row[j*3+1]<<8)
                    | (row[j*3+2]<<16);
            }
        }
        return;
    }

    case 4:
    {
        for (i=0; i<h; i++) {
            png_bytep row = rows[i];
            uint32_t *outptr = output + w * i;

            for (j=0; j<w; j++) {
                /* Premultiply allpha. */
                unsigned r, g, b, alpha, src;
                r = row[j*4+0];
                g = row[j*4+1];
                b = row[j*4+2];
                alpha = row[j*4+3];
                src = row[j*4];
                png_composite(r, src, alpha, 0);
                src = row[j*4+1];
                png_composite(g, src, alpha, 0);
                src = row[j*4+2];
                png_composite(b, src, alpha, 0);
                outptr[j] = (alpha<<24) | (b<<16) | (g<<8) | r;
            }
        }
        return;;
    }

    default:
        fprintf(stderr, "pr->channels is %i. This is unexpected.\n",
                pr->channels);
        return;
    }
}

int
compare_png_signature (uint8_t header[8])
{
    return !png_sig_cmp(header, 0, 8);
}

/* ------------------------------------------------------------------ */

#ifdef BUILD_TEST_PROGRAM

void load_png_file_test (const char *filename)
{
    struct png_reader *pr;

    printf("\"%s\"\n", filename);
    pr = open_and_decode_png_file(filename);

    if (!pr)
    {
        printf("Couldn't open file.\n");
    }
    else
    {
        printf("%4i x %4i x %i\n",
               pr->width,
               pr->height,
               pr->channels);

        free_png_reader(pr);
    }
}

int main (int argc, char **argv)
{
    int i;
    for (i=1; i<argc; i++)
    {
        load_png_file_test(argv[i]);
    }
}

#endif
