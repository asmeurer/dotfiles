import sys

try:
    from IPython.core.prompts import LazyEvaluate
    from iterm2_tools import (BEFORE_PROMPT, AFTER_PROMPT, before_output,
        after_output)

    ipython = get_ipython()

    @LazyEvaluate
    def ipython_before_prompt():
        before_prompt()
        return ''

    @LazyEvaluate
    def ipython_after_prompt():
        after_prompt()
        return ''

    @LazyEvaluate
    def ipython_before_output():
        before_output()
        return ''

    @LazyEvaluate
    def ipython_after_output():
        after_output(bool(sys.exc_info()[0]))
        return ''

    ipython.prompt_manager.lazy_evaluate_fields['before_prompt'] = ipython_before_prompt
    ipython.prompt_manager.lazy_evaluate_fields['after_prompt'] = AFTER_PROMPT
    ipython.prompt_manager.lazy_evaluate_fields['before_output'] = ipython_before_output
    ipython.prompt_manager.lazy_evaluate_fields['after_output'] = ipython_after_output

    orig_in_template = ipython.prompt_manager.in_template
    ipython.prompt_manager.in_template = ("\001" + BEFORE_PROMPT + "\002" + ipython.prompt_manager.in_template +
        "\001" + AFTER_PROMPT + "\002")

    def iterm_pre_prompt_hook(self):
        after_output(0)

    ipython.set_hook('pre_prompt_hook', iterm_pre_prompt_hook)

    class iTermCallback(object):
        def __init__(self, ip):
            self.shell = ip

        def pre_execute(self):
            before_output()

    def load_ipython_extension(ip):
        vw = iTermCallback(ip)
        ip.events.register('pre_execute', vw.pre_execute)

    load_ipython_extension(ipython)

except ImportError:
    raise
