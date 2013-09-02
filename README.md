An Introduction to Functional Programming.

The version presented at the University of Pretoria to the COS-301 students can be found here:
https://www.dropbox.com/s/ot41s13dzacgayd/fp_intro2013-09-02.pdf

A presentation in LaTeX beamer. You need pdflatex to generate the pdf.

If you have rake installed you can generate the pdf by typing "rake" in the top-level directory of this repo.

I used BasicTex, a subset of the MacTex distribution, and then installed the following packages seperately:

```bash
$ sudo easy_install Pygments
$ sudo tlmgr install minted
$ sudo tlmgr install ifplatform
```

http://tug.org/mactex/morepackages.html
