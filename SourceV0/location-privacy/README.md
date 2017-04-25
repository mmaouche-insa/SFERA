About this repository
=====================

[![Build Status](https://travis-ci.com/pvcnt/location-privacy.svg?token=iq89JpmicdUts87rvWPk&branch=master)](https://travis-ci.com/pvcnt/location-privacy)

This repository contains the code of all experiments ran during my PhD thesis.
It provides several modules, some of them that could be reused outside of the context of my thesis, some of them tightly related to what I am doing, i.e., location privacy.
It does *not* contain the source code of my papers.


Repository organisation
-----------------------
The code can be found under the `src/` directory, tests are under the `test` directory.
[Pants](http://www.pantsbuild.org) is used as the build tool.
The main modules are roughly the following.

  - `fr/cnrs/liris/accio`: command-line tool automating experiments, by configuring, launching them and creating visual reports.
  It only contains the framework, not the implementation of the various operators.
  - `fr/cnrs/liris/privamov/core`:  contains implementation of domain objects and of some generic algorithms. Independent from Accio.
  - `fr/cnrs/liris/privamov/ops`:  contains implementation of Accio operators using core algorithms and new algorithms.
  - `fr/cnrs/liris/privamov/gateway`: query service providing access to Priva'Mov data (older version).
  - `fr/cnrs/liris/privamov/query`: query service providing access to Priva'Mov data (newer version, WIP).
  - `fr/cnrs/liris/privamov/indexer`: job indexing a dataset into Elasticsearch, to make it available to the query service
  - `fr/cnrs/liris/common`: generic code used throughout the other modules. It is somewhat generic and not coupled with other specific code.
  - `com/twitter/querulous`: code of Twitter's Querulous library, with some additions (notably to handle PostgreSQL).
  - `com/google/common/geometry`: code of Google'S2 library, which is otherwise not available on Maven.

Except for code borrowed from other projects, everything else is distributed under the french CeCILL
license, largely compatible with the GNU GPL license.

More documentation about Accio can be found under the `docs` directory.
It is used to generate a Jekyll website.
Configuration files, including infrastructure management, can be found under the `etc` directory.
A Vagrant environment including all necessary tools to compile the code is available.


Appendix: My thesis' topic
--------------------------
The goal of this thesis is to study the threats related to the increasing amount of geolocated data.
A first problem is the conception of privacy-preserving geolocated systems.
Actual technologies (e.g. bluetooth, WiFi) rely on protocols leaking sensitive pieces of information about their users.
This can lead to the identification of important places for people (home, work, etc.), prediction of future movements, learning of journeys’ semantic or inference of social relationships.

A second problem is the conception of privacy-preserving data dissemination system in a mobility context.
The architecture provided by the traces collecting system deployed in the Priva'Mov project, to which this thesis is related, will allow to test the efficiency of proposed solutions in a real environment.

A third problem is the investigation of privacy-preserving access mechanisms to mobility traces.
We want to propose efficient techniques to anonymize and access sequential data while offering strong anonymity guarantees.
This requires to find a trade-off between data utility for data analysts and privacy protection for people appearing in traces.

More information and a list of publications can be found on the [Priva'Mov website](http://liris.cnrs.fr/privamov/) or [my personal webpage](http://liris.cnrs.fr/~vprimaul/).