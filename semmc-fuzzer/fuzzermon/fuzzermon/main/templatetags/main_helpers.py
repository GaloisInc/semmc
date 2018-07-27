from django import template
from django.utils.safestring import mark_safe
from django.utils.html import conditional_escape

register = template.Library()

def group(s, chunk_size):
    if len(s) < chunk_size:
        return (('0' * (chunk_size - len(s))) + s)
    else:
        chunk = ""
        chunkStart = len(s) - chunk_size
        for i in range(chunkStart, len(s)):
            chunk += s[i]

        return (group(s[0:chunkStart]) + " " + chunk, chunk_size)

def num(value, ty='dec'):
    inbase = 10
    if value.startswith("0x"):
        inbase = 16

    try:
        if ty == 'dec':
            return str(int(value, inbase))
        elif ty == 'bin':
            return group(bin(int(value, inbase))[2:], 8)
        elif ty == 'hex':
            return group(hex(int(value, inbase))[2:], 4)
        else:
            return str(value)
    except ValueError:
        return str(value)

@register.filter(needs_autoescape=True)
def mono(value, autoescape=True):
    if autoescape:
        esc = conditional_escape
    else:
        esc = lambda x: x

    return mark_safe("<span class=\"mono\">%s</span>" % (esc(value),))

register.filter('num', num)
register.filter('mono', mono)
