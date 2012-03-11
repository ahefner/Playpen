#ifndef PLAYPEN_WINDOWING_H
#define PLAYPEN_WINDOWING_H

#include <X11/X.h>
#include <X11/extensions/Xrender.h>
#include <GL/gl.h>
#include <GL/glx.h>
#include <GL/glu.h>

struct display_info {
    int width, height;

    /* Indicates whether the window system can utilize the OpenGL
       alpha channel. True if nonzero, and in that case a compositing
       window manager should be running for best results.
       (FIXME: How to detect compositing running?) */
    int supports_compositing;
};

#ifndef __PWIN_BACKEND
extern struct display_info dpy_info;
#else
struct display_info dpy_info;
#endif

int pwin_init (char *display_name);

struct window {
    int width, height;
    /* Pointer for user data. Initialized to NULL, never modified. */
    void *user;
    /* Backend data. Shouldn't be defined in this header. Fix later.*/
    Window window;
    //GLXWindow glx_window;
};

enum window_type { Frame = 1 /*, Menu, ... */};


/* Note regarding app_name - presently, this becomes both the window/icon
   title and window class. If you require a different or more detailed
   window title, you can set it separately using window_set_title. */

struct window *create_window (enum window_type type, char *app_name,
                              int width, int height);
void destroy_window (struct window *window);
void window_begin_paint (struct window *window);
void window_end_paint (struct window *window);
void scribble_window (struct window *window);
void window_set_title (struct window *window, char *new_title);

enum event_type {
    /* Pointer Events */
    ev_Motion = 1,
    ev_PointerEnter, ev_PointerExit,
    ev_ButtonPress, ev_ButtonRelease,
    /* Keyboard Events */
    ev_KeyPress, ev_KeyRelease,
    /* Window state events */
    ev_Mapped, ev_Unmapped,
    ev_CloseReq,
    ev_Expose, ev_Resized,
    /* Other Events */
    ev_Timeout
};

struct event {
    enum event_type type;
    struct window *window;

    /* Pointer state */
    unsigned x, y;              /* also used for expose event */
    unsigned button;            /* button # pressed */
    unsigned new_button_state;
    unsigned old_button_state;


    /* Keyboard state */
    unsigned modifier_mask; /* defined for all keyboard and pointer events */
    /* codes/syms defined for KeyPress and KeyRelease */
    unsigned native_code;       /* Window system keycode */
    unsigned native_keysym;     /* Window system keysym */
    unsigned keysym;            /* Translated keysym */
    long unicode;               /* Unicode character code, or -1 if untranslatable */

    /* ev_Expsose and ev_Resized events */
    unsigned width, height;

    /* ev_Resized events also report the old width/height */
    unsigned old_width, old_height;
};

/* Reading events. 'deadline' specified in microseconds, as determined
 * by gettimeofday (via usectime function). */

void
get_event_blocking (struct event *event_out);

void
get_event_with_deadline (struct event *event_out, long long deadline);

#endif
