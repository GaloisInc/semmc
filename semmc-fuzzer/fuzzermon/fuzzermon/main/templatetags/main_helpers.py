from django import template
from django.utils.safestring import mark_safe
from django.utils.html import conditional_escape

register = template.Library()

def num(value, ty='dec'):
    try:
        if ty == 'dec':
            return str(int(value))
        elif ty == 'bin':
            return bin(int(value))
        elif ty == 'hex':
            return hex(int(value))
        else:
            return str(ty)
    except ValueError:
        return str(ty)

@register.filter(needs_autoescape=True)
def mono(value, autoescape=True):
    if autoescape:
        esc = conditional_escape
    else:
        esc = lambda x: x

    return mark_safe("<span class=\"mono\">%s</span>" % (esc(value),))

register.filter('num', num)
register.filter('mono', mono)
