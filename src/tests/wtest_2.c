
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "windowing.h"

float x = 0.5, y = 0.5;

int use_both_windows = 1;

struct extra_info {
    int contains_pointer;
};

unsigned num_windows = 0;
struct window **windows = NULL;

struct extra_info *new_extra (void)
{
    struct extra_info *ex = malloc(sizeof *ex);
    assert(ex != NULL);
    ex->contains_pointer = 0;
    return ex;
}

void draw_circle (float x, float y, float z, float radius, int num_segments)
{
    int i;
    float theta = 0.0, dtheta = 2.0 * M_PI / (float)num_segments;

    glBegin(GL_TRIANGLE_FAN);
    glVertex3f(x,y,z);
    for (i=0; i<=num_segments; i++) {
        glVertex3f(x + radius * cos(theta), y + radius * sin(theta), z);
        theta += dtheta;
    }
    
    glEnd();
}

void paint (struct window *win)
{
    struct extra_info *ex = (struct extra_info *)win->user;
    window_begin_paint(win);

    if (ex->contains_pointer) glClearColor (0.0, 0.3, 0.0, 0.2);
    else glClearColor (0.0, 0.0, 0.0, 0.0);
    glClear (GL_COLOR_BUFFER_BIT);

    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glColor4f(1.0, 1.0, 1.0, 1.0);
    draw_circle(x, y, 0.0, 0.2, 40);
    glColor4f(1.0, 0.0, 0.0, 1.0);
    draw_circle(x, y, 0.0, 0.19, 40);

    window_end_paint(win);
}

struct window *
new_test_window (int width, int height)
{
    struct window **old_windows = windows;    
    struct window *new_window;
    unsigned i;

    windows = malloc(sizeof(struct window *) * (num_windows+1));
    assert(windows != NULL);
    for (i=0; i < num_windows; i++) windows[i] = old_windows[i];
    new_window = create_window(Frame, "wtest2", width, height);
    if (old_windows) free(old_windows);
    if (new_window) {
        new_window->user = new_extra();
        windows[num_windows] = new_window;
        num_windows++;
        return new_window;
    } else return NULL;
}

void remove_window (struct window *win)
{
    unsigned i;

    destroy_window(win);

    for (i=0; i<num_windows; i++) {
        if (windows[i] == win) {
            num_windows--;
            if (num_windows) windows[i] = windows[num_windows];
            else {
                free(windows);
                windows = NULL;
            }
            return;
        }
    }
    assert(0);
}

void paint_all (void)
{
    unsigned i;
    for (i=0; i<num_windows; i++) paint(windows[i]);
}

void destroy_all (void)
{
    if (num_windows != 0) printf("Odd, why are there still windows alive?\n");
    while (num_windows) remove_window(windows[0]);
}

void run_testloop (void)
{
    struct event ev;
    struct extra_info *ex;
    int need_repaint = 1;
  
    while (num_windows)
    {
        /* Get next event without blocking. If we receive an ev_Timeout
           (meaning no real event was available), repaint if necessary.
           Then process all the events in the queue. This is done so that
           if painting takes a long time (due either to slow graphics,
           or glxSwapBuffers doing vsync), the display will not lag behind
           the cursor.
        */
        get_event(&ev, 0);      /* Timeout = 0: Don't block. */
        if (ev.type == ev_Timeout) {
            if (need_repaint) paint_all();
            need_repaint = 0;
            get_event(&ev, -1); /* Timeout = -1: Block for next event */
        }

        if (!ev.window) continue;
        ex = (struct extra_info *) ev.window->user;

        switch (ev.type) {
        
        case ev_Expose:
            // We don't repaint immediately here because it causes paint
            // to lag behind 
            need_repaint = 1;
            break;

        case ev_PointerEnter:
            ex->contains_pointer = 1;
            break;

        case ev_PointerExit:
            ex->contains_pointer = 0;
            need_repaint = 1;
            break;

        case ev_Motion:
            x = (ev.x / (float)ev.window->width) * 2.0 - 1.0;
            y = (ev.y / (float)ev.window->height) * -2.0 + 1.0;
            need_repaint = 1;
            break;

        case ev_ButtonPress:
            printf("Button Press: x=%i y=%i buttons=%X modifiers=%X\n", 
                   ev.x, ev.y, ev.button_state, ev.modifier_mask);
            break;

        case ev_ButtonRelease:
            printf("Button Release: x=%i y=%i buttons=%X modifiers=%X\n", 
                   ev.x, ev.y, ev.button_state, ev.modifier_mask);
            window_set_title(new_test_window(400,400), "Additional Window");
            break;

        case ev_KeyPress:
            printf("Key Press: keysym=%i unicode=%i native_code=%i native_sym=%i modifiers=%X\n", 
                   ev.keysym, ev.unicode, ev.native_code, ev.native_keysym, ev.modifier_mask);
            break;

        case ev_KeyRelease:
            printf("Key Release: keysym=%i unicode=%i native_code=%i native_sym=%i modifiers=%X\n", 
                   ev.keysym, ev.unicode, ev.native_code, ev.native_keysym, ev.modifier_mask);
            break;
            
        case ev_CloseReq:
            printf("Close request, window %p\n", ev.window);
            remove_window(ev.window);
            break;
            
        default: 
            printf("An event arriveth: type=%i, window=%p\n", (int)ev.type, ev.window);
            break;
        }
    }
}

int main (int argc, char **argv)
{
    if (pwin_init(""))
    {
        printf("Window system init failed.\n");
        return 1;
    } 
    else 
    {
        struct window *initial_window;
        
        printf("Root screen is %ix%i, %s alpha\n", 
               dpy_info.width, dpy_info.height, 
               dpy_info.supports_alpha? "supports" : "does not support");

        initial_window = new_test_window(dpy_info.width/2, dpy_info.height/2);
        if (!initial_window) {
            printf("Unable to create initial window\n");
            return 1;
        } else window_set_title(initial_window, "First Window");

        paint_all();
        run_testloop();
        destroy_all();
    }
}
