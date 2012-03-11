#include <stdio.h>
#include "windowing.h"

int main (int argc, char **argv)
{
    if (pwin_init("")) {
        printf("Window system init failed.\n");
    } else {
        int i;
        
        printf("Root screen is %ix%i, %s alpha\n", 
               dpy_info.width, dpy_info.height, 
               dpy_info.supports_alpha? "supports" : "does not support");
        for (i=1; i <= 3; i++) {
            struct window *w;
            printf("Trying window %i.\n", i);
            w = create_window(Frame, "Test window", 600, 400);
            scribble_window(w);
            sleep(1);
            destroy_window(w);
        }
    }
}
