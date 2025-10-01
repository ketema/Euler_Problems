# JDK Setup for Kotlin/Gradle

## Current Status

The system now has OpenJDK 21 installed and configured as the default Java version.

```sh
$ java -version
openjdk version "21.0.8" 2025-07-15
OpenJDK Runtime Environment Homebrew (build 21.0.8)
OpenJDK 64-Bit Server VM Homebrew (build 21.0.8, mixed mode, sharing)
```

## Why JDK 21?

- **Gradle 9.1** requires JVM 17 or later
- **Kotlin 2.2.20** works best with modern JDK versions
- **OpenJDK 21** is a Long-Term Support (LTS) release

## How to Make JDK 21 Default (if needed)

If you need to explicitly set JDK 21 as default:

### Option 1: Symlink (System-wide, requires sudo)

```sh
sudo ln -sfn /opt/homebrew/opt/openjdk@21/libexec/openjdk.jdk \
  /Library/Java/JavaVirtualMachines/openjdk-21.jdk
```

### Option 2: Shell Profile (User-specific, recommended)

Add to `~/.bash_profile` or `~/.zshrc`:

```sh
export JAVA_HOME=/opt/homebrew/opt/openjdk@21
export PATH="$JAVA_HOME/bin:$PATH"
```

Then reload:
```sh
source ~/.bash_profile  # or ~/.zshrc
```

### Option 3: Per-Project (Gradle-specific)

Create `gradle.properties` in project root:

```properties
org.gradle.java.home=/opt/homebrew/opt/openjdk@21
```

### Option 4: Environment Variable (Temporary)

```sh
export JAVA_HOME=/opt/homebrew/opt/openjdk@21
gradle test
```

## Risks of Changing Default JDK

### Low Risk ✅
- **Modern applications**: Most modern Java/Kotlin/Scala apps work fine with JDK 21
- **Gradle/Maven**: Build tools handle JDK versions well
- **Backward compatibility**: JDK 21 can compile code targeting older versions

### Medium Risk ⚠️
- **Legacy applications**: Apps built for Java 8 might have issues
  - Solution: Use `jenv` or `SDKMAN!` to manage multiple JDK versions
- **IDE integration**: Some IDEs might need reconfiguration
  - IntelliJ IDEA: File → Project Structure → SDK
  - VS Code: Update `java.home` in settings

### High Risk ❌
- **Production systems**: Don't change JDK on production without testing
- **Enterprise apps**: Some enterprise frameworks have strict JDK requirements
- **Android development**: Android Studio manages its own JDK

## Managing Multiple JDK Versions

If you need multiple JDK versions:

### Using jenv (recommended)

```sh
# Install jenv
brew install jenv

# Add to shell profile
echo 'export PATH="$HOME/.jenv/bin:$PATH"' >> ~/.bash_profile
echo 'eval "$(jenv init -)"' >> ~/.bash_profile

# Add JDK versions
jenv add /opt/homebrew/opt/openjdk@21
jenv add /Library/Java/JavaVirtualMachines/jdk1.8.0_131.jdk/Contents/Home

# Set global default
jenv global 21.0

# Set per-directory version
cd /path/to/project
jenv local 1.8
```

### Using SDKMAN!

```sh
# Install SDKMAN!
curl -s "https://get.sdkman.io" | bash

# Install JDK versions
sdk install java 21.0.8-tem
sdk install java 8.0.131-oracle

# Switch versions
sdk use java 21.0.8-tem
sdk default java 21.0.8-tem
```

## Verification

Check which Java is being used:

```sh
# System default
java -version

# Gradle's Java
gradle -version | grep JVM

# Maven's Java
mvn -version | grep Java

# Environment variable
echo $JAVA_HOME
```

## Recommendation

**For this project**: The current setup (JDK 21 as default) is optimal and low-risk.

**For your system**: If you have other Java projects:
1. Check if they work with JDK 21
2. If not, use `jenv` or `SDKMAN!` to manage multiple versions
3. Keep JDK 21 as default for new projects

