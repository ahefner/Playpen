#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>
#include <stdint.h>
#include <sys/types.h>
#include <unistd.h>
#include <assert.h>

#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_GLYPH_H

#define max(x,y) ((x)>(y)?(x):(y))
#define min(x,y) ((x)>(y)?(y):(x))

static inline uint8_t clamp_byte (int x)
{
    if (x < 0) return 0;
    if (x > 255) return 255;
    else return x;
}

FT_Library ftlibrary;
int freetype_init = 0;

/* Ensure Freetype library is initialized. */
int text_init (void)
{
    if (!freetype_init) {
        int error = FT_Init_FreeType(&ftlibrary);

        if (error) {
            fprintf(stderr, "Unable to initialize freetype.\n");
            return 0;
        }

        freetype_init = 1;
    }

    return freetype_init;
}

/* Load font from file, returning a Freetype FT_Face pointer. */
FT_Face text_load_face (const char *filename, int facenum)
{
    FT_Face face;

    assert(freetype_init);

    if (FT_New_Face(ftlibrary, filename, facenum, &face)) {
        fprintf(stderr, "Error opening %s\n", filename);
        return NULL;
    } else {
        FT_Select_Charmap(face, FT_ENCODING_UNICODE);
        return face;
    }
}


/** Glyph rendering: **/

struct glyph
{
    unsigned index;
    int width, height, bitmap_left, bitmap_top, pitch;
    int advance_x, advance_y;
    float float_advance_x, float_advance_y;
    void *buffer;
};

struct glyph *
text_render_glyph (FT_Face face, unsigned text_height, unsigned code, int convert_to_rgba)
{
    struct glyph *gl = calloc(1,sizeof(*gl));
    assert(gl!=NULL);

    FT_Set_Pixel_Sizes(face, 0, text_height);
    unsigned flags = FT_LOAD_RENDER
        | FT_LOAD_NO_HINTING
        | FT_LOAD_TARGET_NORMAL;
    FT_UInt index = FT_Get_Char_Index(face, code);
    int error = FT_Load_Glyph(face, index, flags);

    if (error) return NULL;

    gl->index = index;
    gl->bitmap_left = face->glyph->bitmap_left;
    gl->bitmap_top = face->glyph->bitmap_top;
    gl->pitch = face->glyph->bitmap.pitch;
    gl->width = face->glyph->bitmap.width;
    gl->height = face->glyph->bitmap.rows;

    /* FIXME, maybe, I think. Where does the 64 come from? Can't it vary? */
    gl->advance_x = face->glyph->advance.x >> 6;
    gl->advance_y = face->glyph->advance.y >> 6;
    gl->float_advance_x = face->glyph->advance.x / 64.0;
    gl->float_advance_y = face->glyph->advance.y / 64.0;
    gl->buffer = NULL;

    if (gl->width * gl->height)
    {
        uint8_t *in = face->glyph->bitmap.buffer;

        if (convert_to_rgba)
        {
            int n = gl->width * gl->height;
            uint32_t *out = malloc(n*4);
            int i;

            for (i=0; i<n; i++) {
                uint32_t alpha = in[i];
                out[i] = alpha | (alpha<<8) | (alpha<<16) | (alpha<<24);
            }

            gl->buffer = out;
        }
        else
        {
            gl->buffer = malloc(gl->width * gl->height);
            memcpy(gl->buffer, in, gl->width * gl->height);
        }
    }

    return gl;
}

#ifdef BUILD_TEST_PROGRAM

int main (int argc, const char **argv)
{
    int i;
    assert(text_init());

    for (i=1; i<argc; i++) {
        const char *filename = argv[i];
        FT_Face face = text_load_face(filename, 0);
        if (face != NULL) {
            int j;
            printf("Loaded %s\n", filename);
            for (j=0; j<1; j++) {
                text_render_glyph(face, 10, 36+j, 1);
            }
            FT_Done_Face(face);
        } else printf("Failed to load %s\n", filename);
    }

    return 0;
}

#endif
