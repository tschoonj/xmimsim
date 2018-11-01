import sys
import os

# based on graphene...

def to_camel_case(text):
    # We only care about xmi-msim types
    if not text.startswith('xmi_') and not text.startswith('Xmi'):
        return text
    
    if text.startswith('xmi_') and not text.startswith('xmi_msim_'):
        text = "xmi_msim_" + text[4:]

    if text.startswith('Xmi') and not text.startswith('XmiMsim'):
        text = "XmiMsim" + text[3:]
        return text

    if text.startswith('Xmi'):
        return text

    res = []
    for token in text.split('_'):
        uc_token = token.title()
        res.append(uc_token)

    return ''.join(res)

if __name__ == '__main__':
    in_text = sys.stdin.read()
    out_text = to_camel_case(in_text)
    if os.environ.get('XMI_DEBUG_BUILD'):
        with open('identfilter.txt', 'a') as f:
            f.write("{} -> {}\n".format(in_text, out_text))
    sys.stdout.write(out_text)
