Release process:

* ./version.rb --ensure-clean
* ./version.rb --ensure-fresh-master
* ./version.rb --version=2.4.0.M1 --version-branch
* ./version.rb --install
* ./version.rb --version=2.4.0-SNAPSHOT

> **NB!** The script MUST be run in the root directory with the cloned repos. Also, I assume you have stripped `lyo.` from every folder name, e.g. `lyo.core` repo is cloned into `core` folder.