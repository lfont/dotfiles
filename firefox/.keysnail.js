// ========================== KeySnail Init File =========================== //

// You can preserve your code in this area when generating the init file using GUI.
// Put all your code except special key, set*key, hook, blacklist.
// ========================================================================= //
//{{%PRESERVE%
// Put your codes here
plugins.options = {
    "tanything_opt.focus_previous_tab": true
};
//}}%PRESERVE%
// ========================================================================= //

// ========================= Special key settings ========================== //

key.quitKey              = "C-g";
key.helpKey              = "<f1>";
key.escapeKey            = "<f5>";
key.macroStartKey        = "<f3>";
key.macroEndKey          = "<f4>";
key.universalArgumentKey = "C-u";
key.negativeArgument1Key = "C--";
key.negativeArgument2Key = "C-M--";
key.negativeArgument3Key = "M--";
key.suspendKey           = "<f2>";

// ================================= Hooks ================================= //

hook.setHook('KeyBoardQuit', function (aEvent) {
    if (key.currentKeySequence.length) return;

    command.closeFindBar();

    let marked = command.marked(aEvent);

    if (util.isCaretEnabled()) {
        if (marked) {
            command.resetMark(aEvent);
        } else {
            if ("blur" in aEvent.target) aEvent.target.blur();

            gBrowser.focus();
            _content.focus();
        }
    } else {
        goDoCommand("cmd_selectNone");
    }

    if (KeySnail.windowType === "navigator:browser" && !marked) {
        key.generateKey(aEvent.originalTarget, KeyEvent.DOM_VK_ESCAPE, true);
    }
});

// ============================= Key bindings ============================== //

key.setGlobalKey(['M-SPC', 'b'], function (ev, arg) {
    ext.exec('tanything', arg, ev);
}, 'view all tabs ');

key.setGlobalKey(['M-SPC', 'k'], function (ev, arg) {
    ext.exec('tanything-close', arg, ev);
}, 'view all tabs with close as default action');

key.setGlobalKey(['M-SPC', ':'], function (ev) {
    command.interpreter();
}, 'Command interpreter');

key.setGlobalKey(['M-SPC', 'x'], function (ev, arg) {
    ext.select(arg, ev);
}, 'List exts and execute selected one');

key.setGlobalKey('C-n', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_DOWN, true);
}, 'Scroll line down');

key.setGlobalKey('C-p', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_UP, true);
}, 'Scroll line up');

key.setGlobalKey('C-f', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_RIGHT, true);
}, 'Scroll right');

key.setGlobalKey('C-b', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_LEFT, true);
}, 'Scroll left');

key.setGlobalKey('C-y', command.yank, 'Paste (Yank)');

key.setGlobalKey('C-w', function (ev) {
    goDoCommand("cmd_copy");
    goDoCommand("cmd_delete");
    command.resetMark(ev);
}, 'Cut current region');

key.setGlobalKey('M-w', function (ev) {
    command.copyRegion(ev);
}, 'Copy selected text');

key.setGlobalKey('C-a', function (ev) {
    command.beginLine(ev);
}, 'Beginning of the line');

key.setGlobalKey('C-e', function (ev) {
    command.endLine(ev);
}, 'End of the line');

key.setViewKey('c', function (ev, arg) {
    ext.exec('hok-yank-foreground-mode', arg, ev);
}, 'Start Hit a Yank foreground mode');

key.setViewKey('F', function (ev, arg) {
    ext.exec('hok-start-background-mode', arg, ev);
}, 'Start Hit a Hint background mode');

key.setViewKey('f', function (ev, arg) {
    ext.exec('hok-start-foreground-mode', arg, ev);
}, 'Start Hit a Hint foreground mode');

key.setViewKey('g', function (ev) {
    BrowserReload();
}, 'Reload the page');

key.setGlobalKey('C-s', function (ev) {
    command.iSearchForwardKs(ev);
}, 'Emacs like incremental search forward', true);

key.setGlobalKey('C-r', function (ev) {
    command.iSearchBackwardKs(ev);
}, 'Emacs like incremental search backward', true);

key.setGlobalKey(['C-x', 'h'], function (ev) {
    goDoCommand("cmd_selectAll");
}, 'Select all');
