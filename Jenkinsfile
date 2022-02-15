pipeline {
	agent any
	tools {
		maven 'apache-maven-latest'
		jdk 'temurin-jdk11-latest'
	}
	stages {
		stage('Build') {
			steps {
				sh '''
					java -version
					mvn -v
				'''
			}
		}
		stage('Sonar') {
			steps{
				withCredentials([string(credentialsId: 'sonarcloud-token', variable: 'SONARCLOUD_TOKEN')]) {
					withSonarQubeEnv('SonarCloud.io') {
						mvn clean verify -B sonar:sonar -Dsonar.projectKey=org.eclipse.lyo -Dsonar.organization=eclipse -Dsonar.host.url=${SONAR_HOST_URL} -Dsonar.login=${SONARCLOUD_TOKEN}
					}
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
