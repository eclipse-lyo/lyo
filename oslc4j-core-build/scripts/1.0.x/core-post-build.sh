STAGING_HOME=/shared/technology/lyo


if [ -d "$STAGING_HOME/staging/oslc4j/1.0.1" ] ; then
   rm -rf $STAGING_HOME/staging/oslc4j/1.0.1/oslc4j
fi
mkdir -p $STAGING_HOME/staging/oslc4j/1.0.1/oslc4j/apidocs
cp OSLC4J/target/oslc4j-core-1.0.1.jar $STAGING_HOME/staging/oslc4j/1.0.1/oslc4j/.
cp OSLC4JJenaProvider/target/oslc4j-jena-provider-1.0.1.jar $STAGING_HOME/staging/oslc4j/1.0.1/oslc4j/.
cp OSLC4JJson4JProvider/target/oslc4j-json4j-provider-1.0.1.jar $STAGING_HOME/staging/oslc4j/1.0.1/oslc4j/.
cp OSLC4JWink/target/oslc4j-wink-1.0.1.jar $STAGING_HOME/staging/oslc4j/1.0.1/oslc4j/.
cp -R OSLC4JCoreRelEng/target/site/apidocs/* $STAGING_HOME/staging/oslc4j/1.0.1/oslc4j/apidocs/.
find $STAGING_HOME/staging -name "*"
