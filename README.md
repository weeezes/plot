# Plot

A simple tool for plotting data in the terminal

![Plot demo](https://raw.githubusercontent.com/weeezes/plot/master/media/plot_demo.gif)

---

# Building

*This doesn't work in Windows.*

Install [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

```
stack build
stack install
```

---

# Usage

```
plot -- <(set -e; for i in {0..100}; do echo "$((-30 + $RANDOM%60))"; done)
plot -- data.txt
```
Press Q to quit.

You can switch between modes with:

* A - area plot
* B - bar plot
* H - histogram
* P - points
