pipeline {
    agent any
    stages {
        stage ('Haskell package') {
            steps {
                sh 'nix build'
            }
        }
    }
}
