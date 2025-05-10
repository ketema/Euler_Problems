plugins {
    kotlin("jvm") version "2.1.20"
    application
}

group = "org.euler.problem11"
version = "1.0"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
}

application {
    mainClass.set("org.euler.problem11.MatrixProductCLIKt")
}

tasks.test {
    useJUnitPlatform()
}
