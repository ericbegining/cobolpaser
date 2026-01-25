package com.example.cb2xml_opi;

import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;
import java.net.URL;
import java.security.CodeSource;

/**
 * 在系統啟動時檢查 cb2xml 的來源路徑
 */
@Component
public class Cb2xmlVersionChecker implements CommandLineRunner {

    @Override
    public void run(String... args) throws Exception {
        try {
            // 透過 ClassLoader 取得 cb2xml 核心類別的資訊
            Class<?> clazz = Class.forName("net.sf.cb2xml.Cb2Xml3");
            CodeSource codeSource = clazz.getProtectionDomain().getCodeSource();
            
            System.out.println("====================================================");
            if (codeSource != null) {
                URL location = codeSource.getLocation();
                System.out.println(" [系統檢查] cb2xml 載入成功！");
                System.out.println(" [檔案路徑] " + location.getPath());
            } else {
                System.out.println(" [系統檢查] 警告：無法確定 cb2xml 的物理路徑。");
            }
            System.out.println("====================================================");
            
        } catch (ClassNotFoundException e) {
            System.err.println("====================================================");
            System.err.println(" [系統錯誤] 找不到 cb2xml 類別！請檢查 pom.xml 依賴。");
            System.err.println("====================================================");
        }
    }
}