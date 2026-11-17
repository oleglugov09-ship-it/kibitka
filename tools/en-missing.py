#!/usr/bin/env python3
"""List Russian code strings that have no translation in lisp/en.lisp.

Comments, docstrings and shaders don't count. Run from the game directory: tools/en-missing.py"""
import re, glob, json, sys
DOC_AT = {'defun':3,'defmacro':3,'defgeneric':3,'defvar':3,'defparameter':3,'defconstant':3,'defshared':4,'defstruct':2,'defpackage':None}
def strings(src):
    out=[]; i=0; n=len(src); stack=[]   # the stack: [head, number of elements]
    def bump():
        if stack: stack[-1][1]+=1
    while i<n:
        c=src[i]
        if c==';':
            while i<n and src[i]!='\n': i+=1
        elif src.startswith('#|',i):
            i=src.index('|#',i)+2
        elif src.startswith('#\\',i):
            i+=3
            while i<n and (src[i].isalnum() or src[i]=='-'): i+=1
            bump()
        elif c=='"':
            j=i+1; buf=[]
            while src[j]!='"':
                if src[j]=='\\': j+=1
                buf.append(src[j]); j+=1
            s=''.join(buf); i=j+1
            head,idx=(stack[-1] if stack else (None,0))
            doc = head in DOC_AT and DOC_AT[head]==idx
            # a defun's docstring: the string right after the argument list; the one in (:documentation "…") as well
            if head==':documentation': doc=True
            if not doc and re.search('[А-Яа-яЁё]',s): out.append(s)
            bump()
        elif c=='(':
            bump(); stack.append([None,0]); i+=1
        elif c==')':
            if stack: stack.pop()
            i+=1
        elif c.isspace() or c in "'`,@": i+=1
        else:
            j=i
            while j<n and not src[j].isspace() and src[j] not in '()";': j+=1
            tok=src[i:j].lower(); i=j
            if stack and stack[-1][1]==0: stack[-1][0]=tok
            bump()
    return out
have=set(strings(open('lisp/en.lisp',encoding='utf-8').read())) | {s for s in []}
EN=open('lisp/en.lisp',encoding='utf-8').read()
n=0
for f in sorted(glob.glob('lisp/*.lisp')):
    if f.endswith(('live.lisp','en.lisp')): continue
    for s in dict.fromkeys(strings(open(f,encoding='utf-8').read())):
        q='"'+s.replace('\\','\\\\').replace('"','\\"')+'"'
        if q not in EN and 'shader_type' not in s:
            n+=1; print(f, '|', s[:100].replace('\n','/'))
print('без перевода:', n)
