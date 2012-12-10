STAGING_DIR=/shared/technology/lyo/staging/oslc4j/1.0.1
RELEASE_DIR=/shared/technology/lyo/release/oslc4j/1.0.1
DEPENDENCY_DIR=/shared/technology/lyo/release/oslc4/1.0/dependencies

if [ -d "$RELEASE_DIR" ] ; then
   rm -rf $RELEASE_DIR
fi
mkdir -p $RELEASE_DIR
mkdir $RELEASE_DIR/dist
mkdir $RELEASE_DIR/lib
mkdir -p $RELEASE_DIR/doc/apidocs
mkdir $RELEASE_DIR/license
mkdir $RELEASE_DIR/ext

cp $DEPENDENCY_DIR/*.jar $RELEASE_DIR/lib/.
cp $STAGING_DIR/oslc4j/*.jar $RELEASE_DIR/dist/.
cp -R $STAGING_DIR/oslc4j/apidocs/* $RELEASE_DIR/doc/apidocs/.

cd $RELEASE_DIR
jar -cvfM $WORKSPACE/org.eclipse.lyo.oslc4j-1.0.1.zip .
tar -czvf $WORKSPACE/org.eclipse.lyo.oslc4j-1.0.1.tar.gz .

cd $WORKSPACE

find $RELEASE_DIR -name "*"
ls $RELEASE_DIR/..
