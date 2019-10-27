+++
title = "Category talk:MPIF90"
description = ""
date = 2014-03-24T20:03:35Z
aliases = []
[extra]
id = 4587
[taxonomies]
categories = []
tags = []
+++

Isn't this just an utilities to use MPI bindings (or what) for MPI in fortran 90? I mean, the language is still Fortran 90, isn't it? And MPI it's just a library, right? ([http://www.mcs.anl.gov/research/projects/mpi/ MPI project page). I propose to keep simply Fortran 90 and consider MPI as library, without the need to populate with mpicc, mpif90, mpiWhatever, which seem to be just utilities to avoid specifying linking options. --[[User:ShinTakezou|ShinTakezou]] 15:23, 26 July 2009 (UTC)
:ShinTakezou is right. From [http://www.open-mpi.org/doc/v1.6/man1/mpif90.1.php#sect5]:
:"mpif90 is a convenience wrappers for the underlying Fortran 90 compiler...mpif90 passes its arguments to the underlying Fortran 90 compiler..." --[[User:AndiPersti|Andreas Perstinger]] ([[User talk:AndiPersti|talk]]) 20:03, 24 March 2014 (UTC)
