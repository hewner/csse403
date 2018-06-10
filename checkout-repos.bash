#!/usr/bin/bash
PROJECT=ElmSpaceWar
mkdir $PROJECT
for LINE in $(cat csse403-201620-allrepos.txt); do
    pushd /tmp
    svn co http://svn.csse.rose-hulman.edu/repos/$LINE/$PROJECT
    popd
    mv /tmp/$PROJECT ./$PROJECT/$LINE
done
