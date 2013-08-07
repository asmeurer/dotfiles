// https://gist.github.com/minrk/5940801 (except with emacs, of course)
$.getScript("/static/components/codemirror/keymap/emacs.js", function() {
    if (! IPython.Cell) return;
    IPython.Cell.options_default.cm_config.keyMap = "emacs";
});
