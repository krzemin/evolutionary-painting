evolutionary-painting
=====================

This is implementation of evolutionary algorithm which sequentially paints better and better approximation of given image.

Based on ideas from http://rogeralsing.com/2008/12/07/genetic-programming-evolution-of-mona-lisa

Example evolution: http://youtu.be/vV8vC5oYSsQ

usage
=====

SBT is needed to run the evolution.

```sbt "runMain EvolutionaryPainting image.png outdir logfile.txt 100000"```

This command will compile the software, read image from file, and perform 1000000 iterations writing every approximation image to outdir (directory should exists).
