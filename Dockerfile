# 使用輕量級的 OpenJDK 映像檔 (請依據你的 Java 版本修改，如 openjdk:17)
FROM openjdk:17-jdk-slim

# 設定工作目錄
WORKDIR /app

# 將你的 jar 檔複製進去 (假設你的 jar 叫 app.jar)
# 如果你是用 Maven/Gradle build 出來的，路徑可能是 target/app.jar
COPY target/*.jar app.jar

# 告訴 Docker 啟動時要執行什麼指令
ENTRYPOINT ["java", "-jar", "app.jar"]