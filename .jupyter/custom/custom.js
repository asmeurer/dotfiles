// http://stackoverflow.com/a/33015443/161801
var cell = Jupyter.notebook.get_selected_cell();
var patch = {
    CodeCell: {
        cm_config: {
            autoCloseBrackets: false,
        }
    }
}
cell.config.update(patch);
