package com.example.cb2xml_opi.test;

import org.junit.jupiter.api.Test;

import com.example.cb2xml_opi.AnalysisController;
import com.example.cb2xml_opi.CopybookResponse;

public class AnalysisControllerTest {

    @Test
    public void testProcessFiles() {
        System.out.println("========== 開始測試 processFiles ==========");

        // 直接實例化 Controller (因為此方法不依賴 Spring Context 的注入)
        AnalysisController controller = new AnalysisController();

        // 執行 processFiles 方法
        // 注意：此方法會嘗試讀取 D:\_Dev\tmpfiles 資料夾
        CopybookResponse response = controller.processFiles();

        // 輸出執行結果
        System.out.println("執行狀態 (Success): " + response.success);
        
        if (response.error != null) {
            System.err.println("錯誤訊息 (Error): " + response.error);
        }
        
        if (response.data != null) {
            System.out.println("回傳資料 (Data): " + response.data);
        }

        System.out.println("========== 測試結束 ==========");
    }

    @Test
    public void testProcessFilesToItem() {
        System.out.println("========== 開始測試 processFilesToItem ==========");

        // 直接實例化 Controller (因為此方法不依賴 Spring Context 的注入)
        AnalysisController controller = new AnalysisController();

        // 執行 processFilesToItem 方法
        // 注意：此方法會嘗試讀取 D:\_Dev\tmpfiles 資料夾
        CopybookResponse response = controller.processFilesToItem();

        // 輸出執行結果
        System.out.println("執行狀態 (Success): " + response.success);
        
        if (response.error != null) {
            System.err.println("錯誤訊息 (Error): " + response.error);
        }
        
        if (response.data != null) {
            System.out.println("回傳資料 (Data): " + response.data);
        }

        System.out.println("========== 測試結束 ==========");
    }

    @Test
    public void testPraseSourceWtRefCopybook() {
        System.out.println("========== 開始測試 praseSourceWtRefCopybook ==========");

        // 直接實例化 Controller (因為此方法不依賴 Spring Context 的注入)
        AnalysisController controller = new AnalysisController();

        // 執行 praseSourceWtRefCopybook 方法
        // 注意：此方法會嘗試讀取 D:\_Dev\tmpfiles 資料夾
        CopybookResponse response = controller.praseSourceWtRefCopybook();

        // 輸出執行結果
        System.out.println("執行狀態 (Success): " + response.success);
        
        if (response.error != null) {
            System.err.println("錯誤訊息 (Error): " + response.error);
        }
        
        if (response.data != null) {
            System.out.println("回傳資料 (Data): " + response.data);
        }

        System.out.println("========== 測試結束 ==========");
    }
}
