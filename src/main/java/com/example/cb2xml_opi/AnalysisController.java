package com.example.cb2xml_opi;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.ICb2XmlBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.ICopybook;
import net.sf.cb2xml.def.IItem;

import org.springframework.web.bind.annotation.*;


import java.io.File;
import java.nio.file.Files;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.prefs.Preferences;


@CrossOrigin(origins = "*") // 允許任何來源的程式呼叫
@RestController
@RequestMapping("/api")
public class AnalysisController {
    
    private String sourceFilePath = "D:\\_Dev\\tmpfiles";
    private String refCopyFilePath = "D:\\\\_Dev\\\\tmpfiles\\\\RefCopyfiles";
    private Preferences prefs = Preferences.userNodeForPackage(AnalysisController.class);

    public AnalysisController() {
        this.sourceFilePath = prefs.get("sourceFilePath", "D:\\_Dev\\tmpfiles");
        this.refCopyFilePath = prefs.get("refCopyFilePath", "D:\\_Dev\\tmpfiles\\RefCopyfiles");
    }

    @PostMapping("/config/setSourceFilePath")
    public CopybookResponse setSourceFilePath(@RequestBody String newPath) {
        File folder = new File(newPath);
        if (!folder.exists() || !folder.isDirectory()) {
             return new CopybookResponse(false, "Directory does not exist: " + newPath);
        }
        this.sourceFilePath = newPath;
        prefs.put("sourceFilePath", newPath);
        return new CopybookResponse(true, "Path updated to: " + newPath);
    }

    @GetMapping("/config/getSourceFilePath")
    public CopybookResponse getSourceFilePath() {
        return new CopybookResponse(true, this.sourceFilePath);
    }

    @PostMapping("/config/setRefCopyFilePath")
    public CopybookResponse setRefCopyFilePath(@RequestBody String newPath) {
        File folder = new File(newPath);
        if (!folder.exists() || !folder.isDirectory()) {
             return new CopybookResponse(false, "Directory does not exist: " + newPath);
        }
        this.refCopyFilePath = newPath;
        prefs.put("refCopyFilePath", newPath);
        return new CopybookResponse(true, "Path updated to: " + newPath);
    }

    @GetMapping("/config/getRefCopyFilePath")
    public CopybookResponse getRefCopyFilePath() {
        return new CopybookResponse(true, this.refCopyFilePath);
    }

    @GetMapping("/getSourceFiles")
    public CopybookResponse getSourceFiles() {
        File folder = new File(this.sourceFilePath);
        if (!folder.exists() || !folder.isDirectory()) {
             return new CopybookResponse(false, "Directory does not exist: " + this.sourceFilePath);
        }
        
        File[] files = folder.listFiles(File::isFile);
        List<String> fileNames = new ArrayList<>();
        if (files != null) {
            for (File file : files) {
                fileNames.add(file.getName());
            }
        }
        return new CopybookResponse(true, fileNames);
    }

   @GetMapping("/getCopybooksFromSourceFile")
    public CopybookResponse GetCopybooksFromSourceFile(@RequestParam String fileName) {
        try {
            File folder = new File(this.sourceFilePath);
            if (!folder.exists() || !folder.isDirectory()) {
                return new CopybookResponse(false, "Directory " + this.sourceFilePath + " does not exist");
            }

            File file = new File(folder, fileName);
            if (!file.exists() || !file.isFile()) {
                return new CopybookResponse(false, "File " + fileName + " does not exist");
            }

            List<Map<String, Object>> resultList = new ArrayList<>();
            String content = Files.readString(file.toPath());

            // 2. 抓取 FILE SECTION. 底下的內容
            java.util.regex.Pattern fileSectionPattern = java.util.regex.Pattern.compile(
                "(?i)^.{0,6}\\s*FILE\\s+SECTION\\.", java.util.regex.Pattern.MULTILINE);
            java.util.regex.Matcher fsMatcher = fileSectionPattern.matcher(content);

            if (fsMatcher.find()) {
                int start = fsMatcher.end();
                int end = content.length();

                java.util.regex.Pattern nextSectionPattern = java.util.regex.Pattern.compile(
                    "(?i)^.{0,6}\\s*(?:WORKING-STORAGE|LOCAL-STORAGE|LINKAGE|REPORT|SCREEN)\\s+SECTION\\.|^.{0,6}\\s*PROCEDURE\\s+DIVISION\\.", 
                    java.util.regex.Pattern.MULTILINE);
                
                java.util.regex.Matcher nsMatcher = nextSectionPattern.matcher(content);
                if (nsMatcher.find(start)) {
                    end = nsMatcher.start();
                }

                String fileSectionContent = content.substring(start, end);
                String[] structures = fileSectionContent.split("(?=(?m)^.{0,6}\\s*(01|FD|SD)\\s)");

                List<String> searchLines = new ArrayList<>();
                java.util.regex.Pattern procPattern = java.util.regex.Pattern.compile("(?i)^.{0,6}\\s*PROCEDURE\\s+DIVISION\\.", java.util.regex.Pattern.MULTILINE);
                java.util.regex.Matcher pdMatcher = procPattern.matcher(content);
                
                if (pdMatcher.find()) {
                    int pdIndex = pdMatcher.start();
                    String beforeProc = content.substring(0, pdIndex);
                    String afterProc = content.substring(pdIndex);
                    String beforeProcBlanked = beforeProc.replaceAll("[^\\n\\r]", " ");
                    String searchContent = beforeProcBlanked + afterProc;
                    searchLines = java.util.Arrays.asList(searchContent.split("\\r?\\n", -1));
                }

                for (String structure : structures) {
                    if (structure.matches("(?s)^.{0,6}\\s*01\\s.*")) {
                        try {
                            ICb2XmlBuilder cb2 = Cb2Xml3.newBuilder(new StringReader(structure), "temp");
                            cb2.setCobolLineFormat(Cb2xmlConstants.FREE_FORMAT);
                            ICopybook copybook = cb2.asCobolItemTree();
                            

                            List<Map<String, Object>> sourceItems = buildTreeWithReferenceCount(copybook.getChildItems(),0,0, "",   searchLines, 0);

                            String rootFieldName = "";
                            if (!copybook.getChildItems().isEmpty()) {
                                rootFieldName = copybook.getChildItems().get(0).getFieldName();
                            }

                            String refFileNameBase = "C" + rootFieldName.replace("-REC", "") ;
                            File refFile = findRefFile(refFileNameBase);
                            
                            List<Map<String, Object>> refItems = new ArrayList<>();
                            String refFileName = "";

                            if (refFile != null) {
                                refFileName = refFile.getName();
                                try {
                                    String refContent = Files.readString(refFile.toPath());
                                    ICb2XmlBuilder cb2Ref = Cb2Xml3.newBuilder(new StringReader(refContent), "tempRef");
                                    cb2Ref.setCobolLineFormat(Cb2xmlConstants.FREE_FORMAT);
                                    ICopybook refCopybook = cb2Ref.asCobolItemTree();
                                    refItems = buildTreeWithReferenceCount(refCopybook.getChildItems(),0,0,"", searchLines, 0);
                                } catch (Exception e) {
                                    System.err.println("Error parsing ref copybook " + refFile.getName() + ": " + e.getMessage());
                                }
                            }

                            Map<String, Object> fileData = new HashMap<>();
                            fileData.put("fileName", file.getName());
                            fileData.put("structureName", rootFieldName);
                            fileData.put("sourceItems", sourceItems);
                            fileData.put("refFileName", refFileName);
                            fileData.put("refItems", refItems);
                            
                            resultList.add(fileData);
                        } catch (Exception e) {
                            System.err.println("Error parsing structure in " + file.getName() + ": " + e.getMessage());
                        }
                    }
                }
            }
            return new CopybookResponse(true, resultList);
        } catch (Exception e) {
            e.printStackTrace();
            return new CopybookResponse(false, "Error: " + e.getMessage());
        }
    }


    @PostMapping("/analysis")
    public CopybookResponse analysis(@RequestBody String content) {
        try {
            // 解析 COBOL Copybook 字串
            Reader reader=new StringReader(content) ;
            ICopybook copybook = Cb2Xml3.newBuilder(reader, "temp-copybook").asCobolItemTree();
            // return copybook; // 返回解析後的物件樹
            return new CopybookResponse(true, copybook);
        } catch (Exception e) {
            // return "解析出錯: " + e.getMessage();
            return new CopybookResponse(false, "解析出錯: " + e.getMessage());
        }
    }

    @GetMapping("/processFiles")
    public CopybookResponse processFiles() {
        try {
            // 1. 到 sourceFilePath 資料夾下去取得資料夾底下的檔案，不含子資料夾
            File folder = new File(this.sourceFilePath);
            if (!folder.exists() || !folder.isDirectory()) {
                return new CopybookResponse(false, "Directory " + this.sourceFilePath + " does not exist");
            }

            File[] files = folder.listFiles(File::isFile);
            List<String> log = new ArrayList<>();

            if (files != null) {
                for (File file : files) {
                    String content = Files.readString(file.toPath());

                    // 2. 抓取 FILE SECTION. 底下的內容
                    // 尋找 FILE SECTION. 的開始 (忽略前6個字元與空白)
                    java.util.regex.Pattern fileSectionPattern = java.util.regex.Pattern.compile(
                        "(?i)^.{0,6}\\s*FILE\\s+SECTION\\.", java.util.regex.Pattern.MULTILINE);
                    java.util.regex.Matcher fsMatcher = fileSectionPattern.matcher(content);

                    if (fsMatcher.find()) {
                        int start = fsMatcher.end();
                        int end = content.length();

                        // 尋找下一個 SECTION 或 DIVISION 的開始位置
                        java.util.regex.Pattern nextSectionPattern = java.util.regex.Pattern.compile(
                            "(?i)^.{0,6}\\s*(?:WORKING-STORAGE|LOCAL-STORAGE|LINKAGE|REPORT|SCREEN)\\s+SECTION\\.|^.{0,6}\\s*PROCEDURE\\s+DIVISION\\.", 
                            java.util.regex.Pattern.MULTILINE);
                        
                        java.util.regex.Matcher nsMatcher = nextSectionPattern.matcher(content);
                        if (nsMatcher.find(start)) {
                            end = nsMatcher.start();
                        }

                        String fileSectionContent = content.substring(start, end);

                        // 3. 分割並提取 01 層級的 Record
                        // 使用 Lookahead Regex 分割 (01, FD, SD)
                        String[] structures = fileSectionContent.split("(?=(?m)^.{0,6}\\s*(01|FD|SD)\\s)");

                        for (String structure : structures) {
                            // 只處理 01 開頭的區塊
                            if (structure.matches("(?s)^.{0,6}\\s*01\\s.*")) {
                                System.out.println("--------------------------------------------------");
                                System.out.println("File: " + file.getName());
                                System.out.println(structure.trim());
                                log.add("Printed structure from " + file.getName());
                            }
                        }
                    }
                }
            }
            return new CopybookResponse(true, log);
        } catch (Exception e) {
            e.printStackTrace();
            return new CopybookResponse(false, "Error: " + e.getMessage());
        }
    }

    @GetMapping("/processFilesToItem")
    public CopybookResponse processFilesToItem() {
        try {
            File folder = new File(this.sourceFilePath);
            if (!folder.exists() || !folder.isDirectory()) {
                return new CopybookResponse(false, "Directory " + this.sourceFilePath + " does not exist");
            }

            File[] files = folder.listFiles(File::isFile);
            List<Map<String, Object>> resultList = new ArrayList<>();

            if (files != null) {
                for (File file : files) {
                    String content = Files.readString(file.toPath());

                    // 2. 抓取 FILE SECTION. 底下的內容
                    java.util.regex.Pattern fileSectionPattern = java.util.regex.Pattern.compile(
                        "(?i)^.{0,6}\\s*FILE\\s+SECTION\\.", java.util.regex.Pattern.MULTILINE);
                    java.util.regex.Matcher fsMatcher = fileSectionPattern.matcher(content);

                    if (fsMatcher.find()) {
                        int start = fsMatcher.end();
                        int end = content.length();

                        java.util.regex.Pattern nextSectionPattern = java.util.regex.Pattern.compile(
                            "(?i)^.{0,6}\\s*(?:WORKING-STORAGE|LOCAL-STORAGE|LINKAGE|REPORT|SCREEN)\\s+SECTION\\.|^.{0,6}\\s*PROCEDURE\\s+DIVISION\\.", 
                            java.util.regex.Pattern.MULTILINE);
                        
                        java.util.regex.Matcher nsMatcher = nextSectionPattern.matcher(content);
                        if (nsMatcher.find(start)) {
                            end = nsMatcher.start();
                        }

                        String fileSectionContent = content.substring(start, end);
                        String[] structures = fileSectionContent.split("(?=(?m)^.{0,6}\\s*(01|FD|SD)\\s)");

                        for (String structure : structures) {
                            if (structure.matches("(?s)^.{0,6}\\s*01\\s.*")) {
                                try {
                                    ICb2XmlBuilder cb2 = Cb2Xml3.newBuilder(new StringReader(structure), "temp");
                                    cb2.setCobolLineFormat(Cb2xmlConstants.FREE_FORMAT);
                                    ICopybook copybook = cb2.asCobolItemTree();
                                    
                                    // 準備搜尋區域：將目前的 structure 從全文中移除 (替換為空白以避免邊界問題，但保留換行符號以維持行號)
                                    String replacement = structure.replaceAll("[^\\n\\r]", " ");
                                    String searchContent = content.replace(structure, replacement);
                                    List<String> searchLines = java.util.Arrays.asList(searchContent.split("\\r?\\n", -1));

                                    Map<String, Object> fileData = new HashMap<>();
                                    fileData.put("fileName", file.getName());
                                    fileData.put("structureName", copybook.getChildItems().get(0).getFieldName());
                                    fileData.put("items", buildTreeWithReferenceCount(copybook.getChildItems(),0,0,"", searchLines, 0));
                                    
                                    resultList.add(fileData);
                                } catch (Exception e) {
                                    System.err.println("Error parsing structure in " + file.getName() + ": " + e.getMessage());
                                }
                            }
                        }
                    }
                }
            }
            return new CopybookResponse(true, resultList);
        } catch (Exception e) {
            e.printStackTrace();
            return new CopybookResponse(false, "Error: " + e.getMessage());
        }
    }

 
    @GetMapping("/praseSourceWtRefCopybook")
    public CopybookResponse praseSourceWtRefCopybook() {
        try {
            File folder = new File(this.sourceFilePath);
            if (!folder.exists() || !folder.isDirectory()) {
                return new CopybookResponse(false, "Directory " + this.sourceFilePath + " does not exist");
            }

            File[] files = folder.listFiles(File::isFile);
            List<Map<String, Object>> resultList = new ArrayList<>();

            if (files != null) {
                for (File file : files) {
                    String content = Files.readString(file.toPath());

                    // 2. 抓取 FILE SECTION. 底下的內容
                    java.util.regex.Pattern fileSectionPattern = java.util.regex.Pattern.compile(
                        "(?i)^.{0,6}\\s*FILE\\s+SECTION\\.", java.util.regex.Pattern.MULTILINE);
                    java.util.regex.Matcher fsMatcher = fileSectionPattern.matcher(content);

                    if (fsMatcher.find()) {
                        int start = fsMatcher.end();
                        int end = content.length();

                        java.util.regex.Pattern nextSectionPattern = java.util.regex.Pattern.compile(
                            "(?i)^.{0,6}\\s*(?:WORKING-STORAGE|LOCAL-STORAGE|LINKAGE|REPORT|SCREEN)\\s+SECTION\\.|^.{0,6}\\s*PROCEDURE\\s+DIVISION\\.", 
                            java.util.regex.Pattern.MULTILINE);
                        
                        java.util.regex.Matcher nsMatcher = nextSectionPattern.matcher(content);
                        if (nsMatcher.find(start)) {
                            end = nsMatcher.start();
                        }

                        String fileSectionContent = content.substring(start, end);
                        String[] structures = fileSectionContent.split("(?=(?m)^.{0,6}\\s*(01|FD|SD)\\s)");

                        for (String structure : structures) {
                            if (structure.matches("(?s)^.{0,6}\\s*01\\s.*")) {
                                try {
                                    ICb2XmlBuilder cb2 = Cb2Xml3.newBuilder(new StringReader(structure), "temp");
                                    cb2.setCobolLineFormat(Cb2xmlConstants.FREE_FORMAT);
                                    ICopybook copybook = cb2.asCobolItemTree();
                                    
                                    // 準備搜尋區域：將目前的 structure 從全文中移除 (替換為空白以避免邊界問題，但保留換行符號以維持行號)
                                    String replacement = structure.replaceAll("[^\\n\\r]", " ");
                                    String searchContent = content.replace(structure, replacement);
                                    List<String> searchLines = java.util.Arrays.asList(searchContent.split("\\r?\\n", -1));

                                    List<Map<String, Object>> sourceItems = buildTreeWithReferenceCount(copybook.getChildItems(),0,0,"", searchLines, 0);

                                    String rootFieldName = "";
                                    if (!copybook.getChildItems().isEmpty()) {
                                        rootFieldName = copybook.getChildItems().get(0).getFieldName();
                                    }

                                    String refFileNameBase = "C" + rootFieldName.replace("-REC", "") ;
                                    File refFile = findRefFile(refFileNameBase);
                                    
                                    List<Map<String, Object>> refItems = new ArrayList<>();
                                    String refFileName = "";

                                    if (refFile != null) {
                                        refFileName = refFile.getName();
                                        try {
                                            String refContent = Files.readString(refFile.toPath());
                                            ICb2XmlBuilder cb2Ref = Cb2Xml3.newBuilder(new StringReader(refContent), "tempRef");
                                            cb2Ref.setCobolLineFormat(Cb2xmlConstants.FREE_FORMAT);
                                            ICopybook refCopybook = cb2Ref.asCobolItemTree();
                                            refItems = buildTreeWithReferenceCount(refCopybook.getChildItems(),0,0,"", searchLines, 0);
                                        } catch (Exception e) {
                                            System.err.println("Error parsing ref copybook " + refFile.getName() + ": " + e.getMessage());
                                        }
                                    }

                                    Map<String, Object> fileData = new HashMap<>();
                                    fileData.put("fileName", file.getName());
                                    fileData.put("structureName", rootFieldName);
                                    fileData.put("sourceItems", sourceItems);
                                    fileData.put("refFileName", refFileName);
                                    fileData.put("refItems", refItems);
                                    
                                    resultList.add(fileData);
                                } catch (Exception e) {
                                    System.err.println("Error parsing structure in " + file.getName() + ": " + e.getMessage());
                                }
                            }
                        }
                    }
                }
            }
            return new CopybookResponse(true, resultList);
        } catch (Exception e) {
            e.printStackTrace();
            return new CopybookResponse(false, "Error: " + e.getMessage());
        }
    }

    private List<Map<String, Object>> buildTreeWithReferenceCount(List<? extends IItem> items, int depth, int parentOffsetShift, String parentIndexSuffix, List<String> searchLines, int inheritedRefCount) {

       List<Map<String, Object>> tree = new ArrayList<>();
        if(items==null) return tree;

        for (IItem item : items) {

            // 1. 決定重複次數 (OCCURS)，若無則為 1
            int occursCount = Math.max(1, item.getOccurs());

            // 2. 取得單一項目的存儲長度 (用於計算展開後的位移)
            int singleLength = item.getStorageLength();

                for (int i = 0; i < occursCount; i++) {

                    // 3. 計算當前索引產生的位移
                    // 公式：父層累積位移 + (當前索引 * 單一項目長度)
                    int currentShift = parentOffsetShift + (i * singleLength);
                    String currentIndexSuffix;

                    // --- 名稱處理邏輯 ---
                    // 優先順序：
                    // A. 如果我自己有 OCCURS -> 使用我的索引 (i)
                    // B. 如果我沒有 OCCURS，但父層有 -> 繼承父層的索引 (indexSuffix)
                    
                    
                    

                    if (item.getOccurs() > 1) {
                        // 情境 A: 我自己是陣列 (例如 FS14-15 OCCURS 10)
                        // 邏輯: 繼承父層的後綴 + 我自己的索引
                        // 結果: "(0)" + "(2)" -> "(0)(2)"
                        int currentOccurIndex= i + 1;                        
                        currentIndexSuffix = parentIndexSuffix + "(" + currentOccurIndex + ")";
                    } else {
                        // 情境 B: 我只是普通欄位 (例如 FS14)
                        // 邏輯: 直接繼承父層的完整後綴
                        // 結果: "(0)(2)" -> "(0)(2)"
                        currentIndexSuffix = parentIndexSuffix;
                    }

                    // 設定顯示名稱                    
                    String displayName = item.getFieldName();
                    
                    // 排除 FILLER 
                    if (!displayName.toUpperCase().contains("FILLER")) {
                        displayName += currentIndexSuffix;
                    }                    
                    

                    Map<String, Object> row = new LinkedHashMap<>();
                    row.put("LevelString", item.getLevelString());
                    row.put("LevelNumber", item.getLevelNumber());
                    //row.put("FieldName", item.getFieldName());
                    row.put("FieldName", displayName);

                    row.put("Position", item.getPosition()+ currentShift);      
                    row.put("EndPosition", item.getPosition() + currentShift + singleLength - 1);

                    // if (item.getOccurs() > 0){
                    //     row.put("EndPosition", item.getPosition()+item.getStorageLength()*item.getOccurs()-1);
                    // }
                    // else{
                    //     row.put("EndPosition", item.getPosition()+item.getStorageLength()-1);
                    // }

                    row.put("StorageLength", item.getStorageLength());

                    // if (item.getOccurs() > 0){
                    //     row.put("TotalStorageLength", item.getStorageLength()*item.getOccurs());
                    // }
                    // else{
                    //     row.put("TotalStorageLength", item.getStorageLength());
                    // }
                    row.put("TotalStorageLength", singleLength);

                    row.put("Picture", item.getPicture());

                    row.put("NumericClass", item.getNumericClass());
                    row.put("DependingOn", item.getDependingOn());
                    row.put("DisplayLength", item.getDisplayLength());
                    row.put("DisplayPosition", item.getDisplayPosition());

                    row.put("Usage", item.getUsage());

                    if (item.getOccurs() > 0){
                        row.put("Occurs", item.getOccurs());
                    }
                    else{
                        row.put("Occurs", -1);
                    }

                    if (item.getOccursMin() > 0){
                        row.put("OccursMin", item.getOccursMin());
                    }
                    else{
                        row.put("OccursMin", -1);
                    }

                    
                    row.put("Justified", item.getJustified());
                    
                    
                    row.put("RedefinesFieldName", item.getRedefinesFieldName());
                    row.put("Scale", item.getScale());


                    

                    row.put("isSync", item.isSync());        
                    

                    row.put("Value", item.getValue());

                    row.put("isFieldRedefined", item.isFieldRedefined());
                    row.put("isFieldRedefines", item.isFieldRedefines());
                    row.put("isInheritedUsage", item.isInheritedUsage());
                    row.put("isBlankWhenZero", item.isBlankWhenZero());
                    row.put("isBlankWhenZero", item.isBlankWhenZero());
                    row.put("SignClause", item.getSignClause());        
                    row.put("RelativeLevel", item.getRelativeLevel());
                    row.put("Depth", depth);
                    
                    // 計算 ReferencedCount
                    int count = 0;
                    List<String> referencedLines = new ArrayList<>();
                    String name = item.getFieldName();
                    if (name != null && !name.trim().isEmpty() && !name.toUpperCase().contains("FILLER")) {
                        referencedLines = findOccurrences(searchLines, name);
                        count = referencedLines.size();
                    }
                    row.put("ReferencedCount", count);
                    row.put("ReferencedLines", referencedLines);
                    row.put("GroupReferencedCount", inheritedRefCount);
                    row.put("TotalReferencedCount", count + inheritedRefCount);


                    // 關鍵點：如果有子項目，遞迴放入 _children 欄位
                    if (item.getChildItems() != null && !item.getChildItems().isEmpty()) {
                        row.put("_children", buildTreeWithReferenceCount(item.getChildItems(),depth+1,currentShift, currentIndexSuffix, searchLines, count + inheritedRefCount));
                        }
                        tree.add(row);
                }
        }

        return tree;       
    }

    private File findRefFile(String baseName) {
        // System.out.println("========== findRefFile ==========");
        // System.out.println(baseName);
        
        File folder = new File(this.refCopyFilePath);
        if (!folder.exists() || !folder.isDirectory()) {
            return null;
        }
        
        File f = new File(folder, baseName);

        // System.out.println(f.getAbsolutePath());

        if (f.exists() && f.isFile()) return f;

        String[] extensions = {".cbl", ".cpy", ".cob", ".txt", ""};
        for (String ext : extensions) {
            f = new File(folder, baseName + ext);
            if (f.exists() && f.isFile()) return f;
        }

        File[] files = folder.listFiles(File::isFile);
        if (files != null) {
            for (File file : files) {
                String name = file.getName();
                int dotIndex = name.lastIndexOf('.');
                String nameNoExt = (dotIndex == -1) ? name : name.substring(0, dotIndex);
                if (nameNoExt.equalsIgnoreCase(baseName)) {
                    return file;
                }
            }
        }
        return null;
    }

    private List<String> findOccurrences(List<String> lines, String word) {
        List<String> results = new ArrayList<>();
        if (lines == null || lines.isEmpty()) return results;
        
        String regex = "(?<![a-zA-Z0-9-])" + java.util.regex.Pattern.quote(word) + "(?![a-zA-Z0-9-])";
        java.util.regex.Pattern p = java.util.regex.Pattern.compile(regex, java.util.regex.Pattern.CASE_INSENSITIVE);
        
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            java.util.regex.Matcher m = p.matcher(line);
            while (m.find()) {
                results.add("Line " + (i + 1) + ": " + line.trim());
            }
        }
        return results;
    }

}
