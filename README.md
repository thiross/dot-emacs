Zale's emacs configure files.
=============================

WSLg / HiDPI
------------

Use an Emacs build configured with `--with-pgtk` for native Wayland rendering.
X11 windows can look blurry when WSLg scales them on a HiDPI display.
Keep WSLg's HiDPI scaling enabled and let GTK follow the monitor scale.
The Linux default font is Fira Code Retina at 9 points; the floating-point font size
in `lisp/custom-packages.el` specifies points rather than fixed pixels.

Launch a PGTK build with `GDK_BACKEND=wayland` under WSLg. Evaluate
`window-system` with `M-:` to check the display backend; it should be `pgtk`.
