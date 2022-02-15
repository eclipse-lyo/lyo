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
