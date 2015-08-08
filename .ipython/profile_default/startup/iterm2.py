import sys

try:
    from IPython.core.prompts import LazyEvaluate
    from iterm2_tools import (BEFORE_PROMPT, AFTER_PROMPT, BEFORE_OUTPUT,
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
    ipython.prompt_manager.lazy_evaluate_fields['after_prompt'] = ipython_after_prompt
    ipython.prompt_manager.lazy_evaluate_fields['before_output'] = ipython_before_output
    ipython.prompt_manager.lazy_evaluate_fields['after_output'] = ipython_after_output

    ipython.prompt_manager.in_template = ("{after_output}" + BEFORE_PROMPT +
        ipython.prompt_manager.in_template + AFTER_PROMPT)
    ipython.prompt_manager.out_template = ipython.prompt_manager.out_template + BEFORE_OUTPUT
except ImportError:
    raise
