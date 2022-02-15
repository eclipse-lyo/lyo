pipeline {
	agent any
	tools {
		maven 'apache-maven-latest'
		jdk 'temurin-jdk11-latest'
	}
	stages {
		stage('Sonar') {
			steps {
				withCredentials([string(credentialsId: 'sonarcloud-token', variable: 'SONARCLOUD_TOKEN')]) {
					withSonarQubeEnv('SonarCloud.io') {
						sh '''
						mvn clean verify -B org.sonarsource.scanner.maven:sonar-maven-plugin:sonar \
							-Dsonar.projectKey=org.eclipse.lyo -Dsonar.organization=eclipse \
							-Dsonar.host.url=${SONAR_HOST_URL} -Dsonar.login=${SONARCLOUD_TOKEN}
						'''
					}
				}
			}
		}
        stage('Deploy') {
            when {
                anyOf {
                    branch 'origin/master'
                    branch 'origin/maint-*'
                }
            }
            steps {
                withCredentials([file(credentialsId: 'secret-subkeys.asc', variable: 'KEYRING')]) {
                    sh 'gpg --batch --import "${KEYRING}"'
                    sh 'for fpr in $(gpg --list-keys --with-colons  | awk -F: \'/fpr:/ {print $10}\' | sort -u); do echo -e "5\ny\n" |  gpg --batch --command-fd 0 --expert --edit-key ${fpr} trust; done'
                }
                sh '''
                mvn -B -fae clean install -DskipTests javadoc:aggregate \
                        -P dev,gpg-sign,!eclipse-deploy,ossrh-deploy
                mvn -B      deploy        -DskipTests -Dmaven.install.skip=true \
                        -P dev,gpg-sign,!eclipse-deploy,ossrh-deploy
                mvn -B      deploy        -DskipTests -Dmaven.install.skip=true \
                        -P dev,gpg-sign,eclipse-deploy
                '''
                // sh 'gpg --verify my-app/target/my-app-1.0-SNAPSHOT.jar.asc'
                sshagent(['git.eclipse.org-bot-ssh']) {
                    sh '''
                    DOCS_HOME=/home/data/httpd/download.eclipse.org/lyo/docs/all
                    VERSION=$(mvn -q \
                        -Dexec.executable="echo" \
                        -Dexec.args='${project.version}' \
                        --non-recursive \
                        org.codehaus.mojo:exec-maven-plugin:1.3.1:exec | tail -n 1 | xargs)
                    # see https://github.com/eclipse/lyo.core/issues/135 for the tail/xargs temp fix

                    ssh genie.lyo@projects-storage.eclipse.org rm -rf $DOCS_HOME/$VERSION
                    ssh genie.lyo@projects-storage.eclipse.org mkdir -p $DOCS_HOME/$VERSION
                    scp -rp target/site/apidocs/ genie.lyo@projects-storage.eclipse.org:$DOCS_HOME/$VERSION
                    '''
                }

            }
        }
        stage('Publish latest Javadocs') {
            when {
                branch 'origin/master'
            }
            steps {
                sshagent(['git.eclipse.org-bot-ssh']) {
                    sh '''
                    DOCS_HOME=/home/data/httpd/download.eclipse.org/lyo/docs/all
                    VERSION=$(mvn -q \
                        -Dexec.executable="echo" \
                        -Dexec.args='${project.version}' \
                        --non-recursive \
                        org.codehaus.mojo:exec-maven-plugin:1.3.1:exec | tail -n 1 | xargs)
                    # see https://github.com/eclipse/lyo.core/issues/135 for the tail/xargs temp fix

                    ssh genie.lyo@projects-storage.eclipse.org rm -rf $DOCS_HOME/latest
                    ssh genie.lyo@projects-storage.eclipse.org mkdir -p $DOCS_HOME/latest
                    scp -rp target/site/apidocs/ genie.lyo@projects-storage.eclipse.org:$DOCS_HOME/latest
                    '''
                }
            }
        }
	}
	post {
		// send a mail on unsuccessful and fixed builds
		unsuccessful { // means unstable || failure || aborted
			emailext subject: 'Build $BUILD_STATUS $PROJECT_NAME #$BUILD_NUMBER!',
			body: '''Check console output at $BUILD_URL to view the results.''',
			recipientProviders: [culprits(), requestor()],
			to: 'andrew+ham@berezovskyi.me'
		}
		fixed { // back to normal
			emailext subject: 'Build $BUILD_STATUS $PROJECT_NAME #$BUILD_NUMBER!',
			body: '''Check console output at $BUILD_URL to view the results.''',
			recipientProviders: [culprits(), requestor()],
			to: 'andrew+ham@berezovskyi.me'
		}
	}
}
