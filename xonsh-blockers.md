Things xonsh does that makes it hard for me to use. Maybe some of them are
bugs? Either way, all need to be fixed or worked around.

Readline mode:

- [ ] Up arrow doesn't go to the previous command when you have some text typed
  already.

Prompt-toolkit mode:

- [ ] Left arrow at the beginning of a line doesn't go back a line.

- [ ] The prompt colors are wrong. https://github.com/scopatz/xonsh/issues/726.
  Not a huge issue but it would be good to have fixed.

- [ ] Tab completing a path in the beginning of a command doesn't work.

Both modes:

- [ ] My emacsclient/e aliases don't work. No idea why. If I type the command
  directly it works.

- [ ] The iTerm2 terminal title shows (python3.5). I believe this can only be
  fixed upstream by iTerm2.
  https://groups.google.com/forum/#!msg/iterm2-discuss/bG6b1CEoWn0/ndyaGnUtBwAJ

- [ ] iTerm2 shell integration. I'll need to implement this.
  https://github.com/scopatz/xonsh/issues/374

- [ ] The colors can be unreadable (especially environment variable blue)

- [ ] Fuzzy cd (bash cdspell)

- [ ] Warning on > clobber (bash noclobber)

- [ ] auto resume (commands of the same name resume instead of starting a new
  process). Not a blocker, but bash has it and it seems like it would be cool.

- [ ] Custom completion (like completion for cdd)
