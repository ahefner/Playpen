/*****************************************************************

  I/O utilities, and some Unix wrappers.

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
#include <unistd.h>
#include <errno.h>

/* Returns the first 16 bytes of a file. Used for determining file
 * format (you'd be surprised how often image files get saved with the
 * wrong extension..) */
int
read_file_signature (const char *filename, uint8_t sig[16])
{
    FILE *in = fopen(filename, "rb");
    if (!in) return 0;

    if (1 == fread(sig,8,1,in)) {
        fclose(in);
        return 1;
    } else {
        fclose(in);
        return 0;
    }
}

#define constant(name) int const_##name (void) { return name; }

//constant(EBADF);
//constant(EAGAIN);

