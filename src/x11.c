/*****************************************************************

  X11/OpenGL windowing substrate.

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

#define __PWIN_BACKEND

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <unistd.h>
#include <sys/select.h>
#include <sys/time.h>

#include <X11/X.h>
#include <X11/Xatom.h>

#include "windowing.h"
#include "keysym2ucs.h"
#include "keysym.h"

static int attribute_list[] =
{ GLX_RENDER_TYPE, GLX_RGBA_BIT,
  GLX_DRAWABLE_TYPE, GLX_WINDOW_BIT,
  GLX_RED_SIZE, 1,
  GLX_GREEN_SIZE, 1,
  GLX_BLUE_SIZE, 1,
  GLX_ALPHA_SIZE, 1,
  GLX_DOUBLEBUFFER, True,
  GLX_DEPTH_SIZE, 1,
  None };

static Bool
is_mapped (Display *display, XEvent *ev, XPointer arg)
{
    return (ev->type == MapNotify) && (ev->xmap.window == (Window)arg);
}

static unsigned initialized = 0;
static Display *display;
static int screen = 0;          /* ... */
static Colormap cmap;
static XVisualInfo *vi;
static GLXContext cx;
static XContext window_ptr_ctx;
static int display_screen;
static GLXFBConfig fbconfig;

static long x11_event_mask =
    StructureNotifyMask |
    ExposureMask        |
    PointerMotionMask   |
    KeyPressMask        |
    KeyReleaseMask      |
    EnterWindowMask     |
    LeaveWindowMask     |
    ButtonPressMask     |
    ButtonReleaseMask;

static int
check_for_compositing_wm (void)
{
    char atom_name[128];
    snprintf(atom_name, sizeof(atom_name), "_NET_WM_CM_S%i", screen);
    return XGetSelectionOwner(display, XInternAtom(display, atom_name, False)) != None;
}

int
pwin_init (char *display_name)
{
    GLXFBConfig *cfgs;
    int num_cfgs = 0;
    int have_alpha = 1;

    if (initialized) return 0;

    display = XOpenDisplay(display_name);
    if (!display) return 1;
    XSynchronize(display, True);

    window_ptr_ctx = XUniqueContext();

    cfgs = glXChooseFBConfig(display, 0, attribute_list, &num_cfgs);
    if (cfgs && num_cfgs) {
        int i;
        int depth = -1, tmp;
        GLXContext context;
        XRenderPictFormat *pictFormat;

        // TODO: Use of an alpha visual should be an application choice.
        // Provide both alpha and non-alpha visuals if the server
        // supports them.

        for (i = 0; i < num_cfgs; i++) {
            vi = glXGetVisualFromFBConfig(display, cfgs[i]);
            if (!vi) continue;
            pictFormat = XRenderFindVisualFormat(display, vi->visual);
            if (!pictFormat) continue;

            if(pictFormat->direct.alphaMask > 0) {
                fbconfig = cfgs[i];
                break;
            }
        }

        if (i == num_cfgs) {
            /* None of the FBConfigs have alpha.  Use a normal (opaque)
             * FBConfig instead */
            have_alpha = 0;
            fbconfig = cfgs[0];
            vi = glXGetVisualFromFBConfig(display, fbconfig);
            pictFormat = XRenderFindVisualFormat(display, vi->visual);
        }

        display_screen = vi->screen;

        glXGetFBConfigAttrib(display, fbconfig, GLX_BUFFER_SIZE, &depth);
        glXGetFBConfigAttrib(display, fbconfig, GLX_DRAWABLE_TYPE, &tmp);
        assert(tmp & GLX_WINDOW_BIT);

        /* create a GLX context */
        cx = glXCreateContext (display, vi, 0, GL_TRUE);
        /* create a color map */
        cmap = XCreateColormap (display, RootWindow (display, display_screen),
                                vi->visual, AllocNone);
    } else return 2;

    initialized = 1;

    dpy_info.width  = DisplayWidth(display, display_screen);
    dpy_info.height = DisplayHeight(display, display_screen);
    dpy_info.supports_compositing = have_alpha && check_for_compositing_wm();

    if (dpy_info.supports_compositing)
        printf("Compositing supported. Cool!\n");

    return 0;
}

struct window *
lookup_window (Window xwin)
{
    struct window *win = NULL;
    XFindContext(display, xwin, window_ptr_ctx, (XPointer *)&win);
    return win;
}

static void make_current (struct window *window)
{
    assert(True == glXMakeCurrent(display, window->window, cx));
    XSync(display, False);
}

struct window *
create_window (enum window_type type, char *app_name,
               int width, int height)
{
    struct window *window;
    Window xwin;
    XSetWindowAttributes swa;
    XEvent event;
    XSizeHints sizehints;
    XClassHint classhint;
    Atom protocols = XInternAtom(display, "WM_DELETE_WINDOW", False);

    assert(type == Frame);

    swa.colormap = cmap;
    swa.border_pixel = 0;
    swa.event_mask = x11_event_mask;

    xwin = XCreateWindow(display, RootWindow (display, display_screen), 0, 0,
                        width, height, 0, vi->depth, InputOutput, vi->visual,
                        CWBorderPixel | CWColormap | CWEventMask, &swa);

    sizehints.flags = 0;
    /* TODO: Provide interface for min/max size and position hints */
    /* TODO: WM Hints (icon, etc.) */

    classhint.res_name = app_name;
    classhint.res_class = app_name; /* Not ideal.. */

    Xutf8SetWMProperties(display, xwin, app_name, app_name,
                         NULL, 0, /* argv/argc for session manager */
                         &sizehints,
                         NULL,  /* WM Hints */
                         &classhint); /* Class Hint */

    //XChangeProperty(display, xwin, XA_WM_PROTOCOLS, XA_WM_HINTS, 4, PropModeReplace,
    XSetWMProtocols(display, xwin, &protocols, 1);

    XMapWindow(display, xwin);

    /* Wait until window is mapped. */
    XPeekIfEvent(display, &event, is_mapped, (XPointer) xwin);

    window = calloc(1, sizeof(*window));
    assert(window);

    window->user = NULL;
    window->window = xwin;
    window->width = width;
    window->height = height;
    XSaveContext(display, xwin, window_ptr_ctx, (XPointer)window);
    assert(lookup_window(xwin) == window);
    make_current(window);

    return window;
}

/* Test code - draw something arbitrary in the window */
void
scribble_window (struct window *window)
{
    int i;

    window_begin_paint(window);

    glClearColor (0.0, 0.0, 0.0, 0.3);
    glClear (GL_COLOR_BUFFER_BIT);

    glBegin(GL_TRIANGLES);
    glColor4f(1.0, 0.0, 0.0, 1.0);
    glVertex2f(0.5, 0.5);
    glVertex2f(-0.5, 0.5);
    glVertex2f(-0.5, -0.5);
    glColor4f(1.0, 1.0, 0.0, 0.5);
    glVertex3f(-1.0, 1.0, 0.1);
    glVertex3f(1.0, 1.0, 0.1);
    glVertex3f(1.0, -1.0, 0.1);
    glEnd();

    window_end_paint(window);
}

void
destroy_window (struct window *window)
{
    XSaveContext(display, window->window, window_ptr_ctx, (XPointer)NULL);
    XDestroyWindow(display, window->window);
    free(window);
}

static int already_in_paint = 0;
static int issued_indirect_warning = 0;

void
window_begin_paint (struct window *window)
{
    assert(!already_in_paint);
    already_in_paint = 1;

    if (!issued_indirect_warning && (glXIsDirect(display, cx) == False)) {
        printf("Not using indirect rendering..\n");
        issued_indirect_warning = 1;
    }

    make_current(window);
    glViewport(0, 0, window->width, window->height);
}

void
window_end_paint (struct window *window)
{
    assert(already_in_paint);
    glFlush();
    glXSwapBuffers(display, window->window);
    already_in_paint = 0;
    // We could release the context here. Don't see much point in it, though.
}

void
window_set_title (struct window *window, char *new_title)
{
    XChangeProperty(display, window->window, XA_WM_NAME, XA_STRING, 8,
                    PropModeReplace, new_title, strlen(new_title));
}

static unsigned
extract_modmask (unsigned state)
{
    return state & 0xFF;
}

static unsigned
extract_button_state (unsigned state)
{
    return (state >> 8) & 0xFF;
}

static enum key_symbol
translate_keysym (KeySym sym)
{

    switch (sym) {
    case XK_Up: return KEY_UP;
    case XK_Down: return KEY_DOWN;
    case XK_Left: return KEY_LEFT;
    case XK_Right: return KEY_RIGHT;
    case XK_F1: return KEY_F1;
    case XK_F2: return KEY_F2;
    case XK_F3: return KEY_F3;
    case XK_F4: return KEY_F4;
    case XK_F5: return KEY_F5;
    case XK_F6: return KEY_F6;
    case XK_F7: return KEY_F7;
    case XK_F8: return KEY_F8;
    case XK_F9: return KEY_F9;
    case XK_F10: return KEY_F10;
    case XK_F11: return KEY_F11;
    case XK_F12: return KEY_F12;
    case XK_Tab: return KEY_TAB;
    case XK_Escape: return KEY_ESCAPE;
    case XK_Return: return KEY_RETURN;
    case XK_Control_L:
    case XK_Control_R:
        return KEY_CONTROL;
    case XK_Meta_L:
    case XK_Meta_R:
        return KEY_META;
    case XK_Alt_L:
    case XK_Alt_R:
        return KEY_ALT;
    case XK_Super_L:
    case XK_Super_R:
        return KEY_SUPER;
    case XK_Hyper_L:
    case XK_Hyper_R:
        return KEY_HYPER;
    case XK_Shift_L:
    case XK_Shift_R:
        return KEY_SHIFT;
    case XK_BackSpace: return KEY_BACKSPACE;
    case XK_Delete: return KEY_DELETE;
    case XK_Home: return KEY_HOME;
    case XK_End: return KEY_END;
    case XK_Prior: return KEY_PAGEUP;
    case XK_Next: return KEY_PAGEDOWN;
    case XK_Print: return KEY_PRINT;
    case XK_Pause: return KEY_PAUSE;

    default: return 0;
    }
}

long long usectime (void)
{
    struct timeval tv;
    if (gettimeofday(&tv, 0)) {
        perror("gettimeofday");
        exit(1);
    }
    return (((long long)tv.tv_sec) * 1000000ll) + ((long long)tv.tv_usec);
}

/* Convert an XEvent to a struct event, returning 1 if a conversion
   has occured, or 0 if the XEvent should be ignored. */
static int
translate_event (XEvent *xev, struct event *event_out)
{
    struct window *win = lookup_window(xev->xany.window);
    char buf[16];
    KeySym sym;
    unsigned newstate;

    memset(event_out, 0, sizeof(*event_out));

    event_out->window = win;

    switch (xev->type) {
    case KeyPress:
    case KeyRelease:
        event_out->type = xev->type == KeyPress? ev_KeyPress : ev_KeyRelease;
        event_out->modifier_mask = extract_modmask(xev->xkey.state);
        event_out->old_button_state = extract_button_state(xev->xkey.state);
        event_out->new_button_state = event_out->old_button_state;
        event_out->native_code = xev->xkey.keycode;
        //event_out->native_keysym = XLookupKeysym(xev, xev->xkey.keycode);
        XLookupString(&xev->xkey, buf, 16, &sym, NULL);
        event_out->native_keysym = sym;

        // I was going to try and go the extra mile here and use
        // XmbLookupString so dead keys and such would work, but I'm not
        // familiar with X11 input methods and lack a way to test it..

        //printf("%s\n", XKeysymToString(sym));
        event_out->keysym = translate_keysym(event_out->native_keysym);
        event_out->unicode = keysym2ucs(event_out->native_keysym);
        return 1;

    case ButtonPress:
        event_out->type = ev_ButtonPress;
        event_out->x = xev->xbutton.x;
        event_out->y = xev->xbutton.y;
        event_out->modifier_mask = extract_modmask(xev->xbutton.state);
        event_out->old_button_state = extract_button_state(xev->xbutton.state);
        event_out->new_button_state = event_out->old_button_state | (1<<(xev->xbutton.button-1));
        event_out->button = xev->xbutton.button;
        return 1;

    case ButtonRelease:
        event_out->type = ev_ButtonRelease;
        event_out->x = xev->xbutton.x;
        event_out->y = xev->xbutton.y;
        event_out->modifier_mask = extract_modmask(xev->xbutton.state);
        event_out->old_button_state = extract_button_state(xev->xbutton.state);
        event_out->new_button_state = event_out->old_button_state & ~(1<<(xev->xbutton.button-1));
        event_out->button = xev->xbutton.button;
        return 1;

    case MotionNotify:
        event_out->type = ev_Motion;
        event_out->x = xev->xmotion.x;
        event_out->y = xev->xmotion.y;
        event_out->modifier_mask = extract_modmask(xev->xmotion.state);
        event_out->old_button_state = extract_button_state(xev->xmotion.state);
        event_out->new_button_state = event_out->old_button_state;
        return 1;

    case EnterNotify:
    case LeaveNotify:
        event_out->type = xev->type == EnterNotify? ev_PointerEnter : ev_PointerExit;
        event_out->x = xev->xcrossing.x;
        event_out->y = xev->xcrossing.y;
        event_out->modifier_mask = extract_modmask(xev->xcrossing.state);
        event_out->old_button_state = extract_button_state(xev->xcrossing.state);
        event_out->new_button_state = event_out->old_button_state;
        return 1;

    case Expose:
        event_out->type = ev_Expose;
        event_out->x = xev->xexpose.x;
        event_out->y = xev->xexpose.y;
        event_out->width = xev->xexpose.width;
        event_out->height = xev->xexpose.height;
        return 1;

    case MappingNotify:
        if (xev->xmapping.request == MappingKeyboard ||
            xev->xmapping.request == MappingModifier)
            XRefreshKeyboardMapping(&xev->xmapping);
        return 0;

    case ConfigureNotify:
        /* Currently only interested in changes in width/height: */
        if (win && (xev->xconfigure.width  != win->width ||
                    xev->xconfigure.height != win->height)) {
            event_out->type = ev_Resized;
            event_out->old_width = win->width;
            event_out->old_height = win->height;
            win->width = xev->xconfigure.width;
            win->height = xev->xconfigure.height;
            event_out->width = win->width;
            event_out->height = win->height;
            return 1;
        } else return 0;

    case ClientMessage:
        if (xev->xclient.message_type == XInternAtom(display, "WM_PROTOCOLS", False))
        {
            //printf("WM_PROTOCOLS message %i\n", (int)xev->xclient.data.l[0]);
            if (xev->xclient.data.l[0] == XInternAtom(display, "WM_DELETE_WINDOW", False))
            {
                event_out->type = ev_CloseReq;
                return 1;
            }
        }
        return 0;

    case DestroyNotify:
    case ReparentNotify:
    case GravityNotify: return 0;


    case MapNotify:
        event_out->type = ev_Mapped;
        return 1;

    case UnmapNotify:
        event_out->type = ev_Unmapped;
        return 1;

    default:
        fprintf(stderr, "Unhandled event of type %i\n", xev->type);
        return 0;
    }
}

static int debugev = 0;

static int
trace_translate_event (XEvent *in, struct event *out)
{
    int tmp = translate_event(in,out);

    if (debugev) {
        if (tmp)
            fprintf(stderr, "translate_event type %i\n", (int)out->type);
        else
            fprintf(stderr, "translate_event rejected event.\n");
    }

    return tmp;
}


static fd_set fdsets[3];
static int max_watched_fd = -1;
#define RDSET (&fdsets[0])
#define WRSET (&fdsets[1])
#define EXSET (&fdsets[2])


/*
void
reset_watched_fds (void)
{
    int i;
    for (i=0; i<3; i++)
        FD_ZERO(&fdsets[i]);
    max_watched_fd = -1;
}

void
watch_fd (int setidx, int fd)
{
    if ((setidx >= 0) && (setidx <= 2) && (fd >= 0)) {
        FD_SET(fd, &fdsets[setidx]);
        if (fd > max_watched_fd) max_watched_fd = fd;
    } else {
        fprintf(stderr, "watch_fd(%i,%i) bad args!\n", setidx, fd);
    }
}
*/

static int
nonblock_get_x_event (struct event *event_out)
{
    XEvent ev;

    /* The intent of using XEventsQueued in this way is to prevent a
     * flood of X11 events from starving out the other file
     * descriptors (TODO). No promises.  */
    while (XEventsQueued(display,QueuedAlready) > 0)
    {
        XNextEvent(display, &ev);
        if (trace_translate_event(&ev, event_out)) return 1;
    }
    if (debugev) fprintf(stderr, "nonblock_get_x_event: rejected everything.\n");
    return 0;
}

static void
get_event_inner (struct event *event_out, int blocking, long long deadline)
{
    XEvent ev;
    int i, tmp;
    int xfd = ConnectionNumber(display), maxfd = xfd;
    long long now, delta;
    struct timeval tv;

    assert(event_out != NULL);
    memset(event_out, 0, sizeof(*event_out));

    if (debugev) fprintf(stderr, " get_event_inner %lli\n", deadline);

    /* Wait for event / deadline */
    while (1)
    {
        /* If there's a translatable X event, return that. */
        if (nonblock_get_x_event(event_out)) return;

        /* Prepare file descriptors for select(2) */
        for (i=0; i<3; i++) FD_ZERO(&fdsets[i]);
        FD_SET(xfd, RDSET);


        if (blocking)
        {
            /** Blocking read. **/
            tmp = select(maxfd+1, RDSET, WRSET, EXSET, NULL);
        }
        else
        {
            /** Deadline read. **/

            /* If we missed the deadline, return a timeout. */
            now = usectime();
            delta = deadline - now;
            if (delta <= 0) {
                event_out->type = ev_Timeout;
                return;
            }

            /* Wait for an event. */
            tv.tv_sec = delta / 1000000ll;
            tv.tv_usec = delta % 1000000ll;
            tmp = select(maxfd+1, RDSET, WRSET, EXSET, &tv);

            if (!tmp)
            {
                /* Reached deadline? */
                event_out->type = ev_Timeout;
                return;
            }
            else if (tmp < 0)
            {
                /* Something is amiss */
                perror("select");
                event_out->type = ev_Timeout; /* ? */
                return;
            }
            else
            {
                /* Otherwise, no timeout and >0 FDs ready. */
                /* Check if it's an X11 event, otherwise return IOReady? */
                //event_out->type = ev_IOReady;
                //fprintf(stderr, "select => %i\n", tmp);
            }
        }

        if (FD_ISSET(xfd, RDSET) && (XPending(display) > 0)) {
            /* Curious.. */
            if (debugev) fprintf(stderr, "  partial X11 event in buffer?\n");
        }

        /* Continue waiting.. */
    }
}

/* This is a nicer way to visualize the behavior of the event loop. */
static int ticks = 0;

void
get_event_with_deadline (struct event *event_out, long long deadline)
{
    if (debugev) fprintf(stderr, "get_event_with_deadline %lli\n", deadline);
    if (ticks) fprintf(stderr, ".");
    get_event_inner(event_out, 0, deadline);
}

void
get_event_blocking (struct event *event_out)
{
    if (debugev) fprintf(stderr, "get_event_blocking\n");
    if (ticks) fprintf(stderr, ":");
    get_event_inner(event_out, 1, 0);
}

