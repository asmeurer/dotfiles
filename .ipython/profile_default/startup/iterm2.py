try:
    from IPython.core.prompts import LazyEvaluate
    from iterm2_tools import (BEFORE_PROMPT, AFTER_PROMPT, before_output,
        after_output)

    ipython = get_ipython()

    global status
    status = 0

    @LazyEvaluate
    def ipython_after_output():
        global status
        after_output(status)
        status = 0
        return ''

    ipython.prompt_manager.lazy_evaluate_fields['before_prompt'] = BEFORE_PROMPT
    ipython.prompt_manager.lazy_evaluate_fields['after_prompt'] = AFTER_PROMPT
    ipython.prompt_manager.lazy_evaluate_fields['after_output'] = ipython_after_output

    orig_in_template = ipython.prompt_manager.in_template
    ipython.prompt_manager.in_template = ("\001{after_output}\002\001{before_prompt}\002" + ipython.prompt_manager.in_template +
        "\001{after_prompt}\002")

    def load_ipython_extension(ip):
        ip.events.register('pre_execute', before_output)

    load_ipython_extension(ipython)

    def exc_handler(self, etype, value, tb, tb_offset=None):
        global status
        status = 1
        return self.showtraceback()

    ipython.set_custom_exc((Exception,), exc_handler)

except ImportError:
    pass
