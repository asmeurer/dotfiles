
$([IPython.events]).on('notebook_loaded.Notebook', function(){

var cmd = IPython.keyboard_manager.command_shortcuts;
var edit = IPython.keyboard_manager.edit_shortcuts;
var def_cmd = IPython.default_command_shortcuts;
var def_edit = IPython.default_edit_shortcuts;

// 'i' by default interrupts the kernel (what Ctrl-C does at the terminal)
edit.remove_shortcut('shift-enter');
edit.add_shortcut('shift-enter', def_edit['alt-enter']);

cmd.remove_shortcut('shift-enter');
cmd.add_shortcut('shift-enter', def_cmd['alt-enter']);
console.log('success');
});
