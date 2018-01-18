from django import template

register = template.Library()

def num(value, ty='dec'):
    if ty == 'dec':
        return str(int(value))
    elif ty == 'bin':
        return bin(int(value))
    elif ty == 'hex':
        return hex(int(value))
    else:
        return str(ty)

register.filter('num', num)
