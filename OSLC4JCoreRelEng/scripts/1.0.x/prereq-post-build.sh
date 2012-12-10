STAGING_HOME=/shared/technology/lyo

if [ -d "$STAGING_HOME/staging/dependencies" ] ; then
   rm -rf $STAGING_HOME/staging/oslc4j/1.0/dependencies
fi
mkdir -p $STAGING_HOME/staging/oslc4j/1.0/dependencies
cp oslc4j/1.0/jena-iri/0.9.2/target/jena-iri-0.9.2.jar $STAGING_HOME/staging/oslc4j/1.0/dependencies/.
cp oslc4j/1.0/jena-core/2.7.1/target/jena-core-2.7.1.jar $STAGING_HOME/staging/oslc4j/1.0/dependencies/.
cp oslc4j/1.0/wink/1.2.1/wink-common/target/wink-common-1.2.1-incubating.jar $STAGING_HOME/staging/oslc4j/1.0/dependencies/.
cp oslc4j/1.0/wink/1.2.1/wink-client/target/wink-client-1.2.1-incubating.jar $STAGING_HOME/staging/oslc4j/1.0/dependencies/.
cp oslc4j/1.0/wink/1.2.1/wink-client-apache-httpclient/target//wink-client-apache-httpclient-1.2.1-incubating.jar $STAGING_HOME/staging/oslc4j/1.0/dependencies/.
cp oslc4j/1.0/wink/1.2.1/wink-json4j/target/wink-json4j-1.2.1-incubating.jar $STAGING_HOME/staging/oslc4j/1.0/dependencies/.
cp oslc4j/1.0/wink/1.2.1/wink-server/target/wink-server-1.2.1-incubating.jar $STAGING_HOME/staging/oslc4j/1.0/dependencies/.
cp oslc4j/1.0/wink/1.2.1/wink-providers/wink-json4j-provider/target/wink-json4j-provider-1.2.1-incubating.jar $STAGING_HOME/staging/oslc4j/1.0/dependencies/.
cp oslc4j/1.0/prereqs/*.jar $STAGING_HOME/staging/oslc4j/1.0/dependencies

find /shared/technology/lyo/staging -name "*"
