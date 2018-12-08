#ifdef _WINDOW_CONFIG

/* Default window dimensions (overwritten via -g option): */
enum {
    WIN_WIDTH  = 800,
    WIN_HEIGHT = 600
};

/* static const char * const BAR_FONT = "-*-liberation mono-medium-r-*-*-13-*-*-*-*-*-*-*"; */
static const char * const BAR_FONT = "-*-dejavu sans mono-medium-r-*-*-13-*-*-*-*-*-*-*";
static const char * const WIN_BG_COLOR = "#777777";
static const char * const WIN_FS_COLOR = "#000000";
static const char * const SEL_COLOR    = "#DDDDDD";
static const char * const BAR_BG_COLOR = "#3F3F3F";
static const char * const BAR_FG_COLOR = "#D5D2BE";

#endif
#ifdef _IMAGE_CONFIG

/* levels (in percent) to use when zooming via '-' and '+':
 * (first/last value is used as min/max zoom level)
 */
static const float zoom_levels[] = {
    12.5,  25.0,  50.0,  75.0,
    100.0, 150.0, 200.0, 400.0, 800.0
};

/* default slideshow delay (in sec, overwritten via -S option): */
enum { SLIDESHOW_DELAY = 5 };

/* gamma correction: the user-visible ranges [-GAMMA_RANGE, 0] and
 * (0, GAMMA_RANGE] are mapped to the ranges [0, 1], and (1, GAMMA_MAX].
 * */
static const double GAMMA_MAX   = 10.0;
static const int    GAMMA_RANGE = 32;

/* Command i_scroll pans image 1/PAN_FRACTION of screen width/height. */
static const int PAN_FRACTION = 5;

/* if false, pixelate images at zoom level != 100%,
 * toggled with 'a' key binding
 */
static const bool ANTI_ALIAS = true;

/* if true, use a checkerboard background for alpha layer,
 * toggled with 'A' key binding
 */
static const bool ALPHA_LAYER = false;

#endif
#ifdef _THUMBS_CONFIG

/* thumbnail sizes in pixels (width == height): */
static const int thumb_sizes[] = { 32, 64, 96, 128, 160 };

/* thumbnail size at startup, index into thumb_sizes[]: */
static const int THUMB_SIZE = 3;

#endif
#ifdef _MAPPINGS_CONFIG

/* Keyboard mappings for image and thumbnail mode: */
static const keymap_t keys[] = {
    /* modifiers    key               function              argument */
    { 0,            XK_q,             g_quit,               None },
    { 0,            XK_Q,             g_quit,               None },
    { 0,            XK_Return,        g_switch_mode,        None },
    { 0,            XK_f,             g_toggle_fullscreen,  None },
    { 0,            XK_b,             g_toggle_bar,         None },
    { ControlMask,  XK_x,             g_prefix_external,    None },
    { 0,            XK_D,             g_remove_image,       None },

    { 0,            XK_g,             g_reload_image,       None },
    { 0,            XK_G,             t_reload_all,         None },

    { 0,            XK_a,             g_first,              None },
    { 0,            XK_i,             g_n_or_last,          None },

    /* Scrolling the image. */
    { ControlMask,  XK_o,             i_scroll,             DIR_LEFT },
    { ControlMask,  XK_Left,          i_scroll,             DIR_LEFT },
    { ControlMask,  XK_e,             i_scroll,             DIR_DOWN },
    { ControlMask,  XK_Down,          i_scroll,             DIR_DOWN },
    { ControlMask,  XK_period,        i_scroll,             DIR_UP },
    { ControlMask,  XK_Up,            i_scroll,             DIR_UP },
    { ControlMask,  XK_u,             i_scroll,             DIR_RIGHT },
    { ControlMask,  XK_Right,         i_scroll,             DIR_RIGHT },
    { 0,            XK_o,             g_scroll_screen,      DIR_LEFT },
    { 0,            XK_Left,          g_scroll_screen,      DIR_LEFT },
    { 0,            XK_e,             g_scroll_screen,      DIR_DOWN },
    { 0,            XK_Down,          g_scroll_screen,      DIR_DOWN },
    { 0,            XK_period,        g_scroll_screen,      DIR_UP },
    { 0,            XK_Up,            g_scroll_screen,      DIR_UP },
    { 0,            XK_u,             g_scroll_screen,      DIR_RIGHT },
    { 0,            XK_Right,         g_scroll_screen,      DIR_RIGHT },
    { 0,            XK_O,             i_scroll_to_edge,     DIR_LEFT },
    { 0,            XK_E,             i_scroll_to_edge,     DIR_DOWN },
    { 0,            XK_greater,       i_scroll_to_edge,     DIR_UP },
    { 0,            XK_U,             i_scroll_to_edge,     DIR_RIGHT },
    /* { 0,            XK_less,          i_scroll_to_edge,     DIR_LEFT | DIR_UP }, */
    /* { 0,            XK_P,             i_scroll_to_edge,     DIR_LEFT | DIR_UP }, */

    /* Rotating the image. */
    { 0,            XK_comma,         i_rotate,             DEGREE_90 },
    { 0,            XK_p,             i_rotate,             DEGREE_270 },
    /* { 0,            XK_question,      i_rotate,             DEGREE_180 }, */
    { 0,            XK_bar,           i_flip,               FLIP_HORIZONTAL },
    { 0,            XK_underscore,    i_flip,               FLIP_VERTICAL },

    /* Moving by the thumbnails. */
    { 0,            XK_o,             t_move_sel,           DIR_LEFT },
    { 0,            XK_Left,          t_move_sel,           DIR_LEFT },
    { 0,            XK_e,             t_move_sel,           DIR_DOWN },
    { 0,            XK_Down,          t_move_sel,           DIR_DOWN },
    { 0,            XK_period,        t_move_sel,           DIR_UP },
    { 0,            XK_Up,            t_move_sel,           DIR_UP },
    { 0,            XK_u,             t_move_sel,           DIR_RIGHT },
    { 0,            XK_Right,         t_move_sel,           DIR_RIGHT },

    { 0,            XK_plus,          g_zoom,               +1 },
    { 0,            XK_KP_Add,        g_zoom,               +1 },
    { 0,            XK_minus,         g_zoom,               -1 },
    { 0,            XK_KP_Subtract,   g_zoom,               -1 },
    { 0,            XK_equal,         i_set_zoom,           100 },

    /* Navigating (moving by the images). */
    { 0,            XK_n,             i_navigate,           +1 },
    { 0,            XK_space,         i_navigate,           +1 },
    { 0,            XK_h,             i_navigate,           -1 },
    { 0,            XK_BackSpace,     i_navigate,           -1 },
    { 0,            XK_bracketright,  i_navigate,           +10 },
    { 0,            XK_bracketleft,   i_navigate,           -10 },
    { ControlMask,  XK_r,             i_alternate,          None },
    { 0,            XK_N,             g_navigate_marked,    +1 },
    { 0,            XK_H,             g_navigate_marked,    -1 },

    /* Marking the images. */
    { 0,            XK_m,             g_toggle_image_mark,  None },
    { 0,            XK_asterisk,      g_reverse_marks,      None },
    { 0,            XK_Z,             g_unmark_all,         None },

    { 0,            XK_braceleft,     g_change_gamma,       -1 },
    { 0,            XK_braceright,    g_change_gamma,       +1 },
    { ControlMask,  XK_g,             g_change_gamma,        0 },

    { ControlMask,  XK_n,             i_navigate_frame,     +1 },
    { ControlMask,  XK_h,             i_navigate_frame,     -1 },
    { ControlMask,  XK_space,         i_toggle_animation,   None },
    { 0,            XK_w,             i_fit_to_win,         SCALE_DOWN },
    { 0,            XK_W,             i_fit_to_win,         SCALE_FIT },
    /* { 0,            XK_e,             i_fit_to_win,         SCALE_WIDTH }, */
    /* { 0,            XK_E,             i_fit_to_win,         SCALE_HEIGHT }, */

    { 0,            XK_A,             i_toggle_antialias,   None },
    /* { 0,            XK_A,             i_toggle_alpha,       None }, */
    { 0,            XK_s,             i_slideshow,          None },
};

/* Mouse button mappings for image mode: */
static const button_t buttons[] = {
    /* modifiers    button            function              argument */
    { 0,            1,                i_navigate,           +1 },
    { 0,            3,                i_navigate,           -1 },
    { 0,            2,                i_drag,               None },
    { 0,            4,                i_scroll,             DIR_UP },
    { 0,            5,                i_scroll,             DIR_DOWN },
    { ShiftMask,    4,                i_scroll,             DIR_LEFT },
    { ShiftMask,    5,                i_scroll,             DIR_RIGHT },
    { 0,            6,                i_scroll,             DIR_LEFT },
    { 0,            7,                i_scroll,             DIR_RIGHT },
    { ControlMask,  4,                g_zoom,               +1 },
    { ControlMask,  5,                g_zoom,               -1 },
};

#endif
