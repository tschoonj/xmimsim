import sys

# based on graphene...

def to_camel_case(text):
    # We only care about xmi-msim types
    if not text.startswith('xmi_'):
        return text
    
    if text.startswith('xmi_') and not text.startswith('xmi_msim_'):
        text = "xmi_msim_" + text[4:]

    return text

if __name__ == '__main__':
    in_text = sys.stdin.read()
    sys.stdout.write(to_camel_case(in_text))

