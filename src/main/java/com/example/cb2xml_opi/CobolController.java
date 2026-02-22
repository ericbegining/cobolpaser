package com.example.cb2xml_opi;

import net.sf.cb2xml.Cb2Xml3;
import net.sf.cb2xml.ICb2XmlBuilder;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cb2xml.def.ICopybook;
import net.sf.cb2xml.def.IItem;

import org.springframework.web.bind.annotation.*;


import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;


@CrossOrigin(origins = "*") // 允許任何來源的程式呼叫
@RestController
@RequestMapping("/api")
public class CobolController {
    

    @PostMapping("/parse")
    public CopybookResponse parse(@RequestBody String content) {
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
    @PostMapping(value = "/parse-tree", consumes = "text/plain")
    public List<FieldDto> parseForTree(@RequestBody String content) {
        try {
            // 解析 COBOL Copybook 字串
            Reader reader=new StringReader(content) ;
            ICopybook copybook = Cb2Xml3.newBuilder(reader, "temp-copybook").asCobolItemTree();
            // return copybook; // 返回解析後的物件樹            
            return  convertToDtoTree(copybook.getChildItems(),-1);
        } catch (Exception e) {
            e.printStackTrace();
            return Collections.emptyList();
        }
    }

    private List<FieldDto> convertToDtoTree(List<? extends IItem> items, int lastExpectedPos) {
        List<FieldDto> dtoList = new ArrayList<>();
        int expectedPos = lastExpectedPos;

        for (IItem item : items) {
            int currentPos = item.getPosition();
            
            // 1. 自動偵測並加入 GAP (如果有)
            if (expectedPos != -1 && currentPos > expectedPos) {
                FieldDto gap = new FieldDto("--", "[隱藏間隙 / FILLER]", expectedPos, currentPos - expectedPos);
                gap.isGap = true;
                dtoList.add(gap);
            }

            // 2. 轉換目前的 Item 為 DTO
            FieldDto dto = new FieldDto(
                item.getLevelString(),
                item.getFieldName(),
                currentPos,
                item.getStorageLength()
            );
            
            dto.picture = item.getPicture(); 
            dto.redefines = item.getRedefinesFieldName(); 
            dto.occurs = item.getOccurs();
            dto.totalLength = (dto.occurs > 0) ? (dto.singleLength * dto.occurs) : dto.singleLength;
            dto.isGap = false;

            // 3. 遞迴處理子項目
            if (item.getChildItems() != null && !item.getChildItems().isEmpty()) {
                dto.children = convertToDtoTree(item.getChildItems(), -1); 
                // 子項目內部的 Gap 由遞迴處理，這裏傳入 -1 重置
            }

            dtoList.add(dto);

            // 4. 更新預期位置 (如果是 REDEFINES 則不推進)
            if (dto.redefines == null || dto.redefines.isEmpty()) {
                expectedPos = currentPos + dto.totalLength;
            }
        }
        return dtoList;
    }
    @PostMapping("/parse-grid")
    public List<Map<String, Object>> parseForGrid(@RequestBody String content) {
        List<Map<String, Object>> gridData = new ArrayList<>();
        try {
            ICopybook copybook = Cb2Xml3.newBuilder(new StringReader(content), "temp").asCobolItemTree();
            // 開始遞迴解析每一個 Item
            flattenItems(copybook.getChildItems(), gridData);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return gridData;
    }
    private void flattenItems(List<? extends IItem> items, List<Map<String, Object>> gridData) {
        for (IItem item : items) {
            Map<String, Object> row = new HashMap<>();
            row.put("level", item.getLevelString());
            row.put("name", item.getFieldName());
            row.put("position", item.getPosition());
            row.put("length", item.getStorageLength());
            row.put("picture", item.getPicture());
            gridData.add(row);
            
            // 如果有子項目，遞迴處理
            if (item.getChildItems() != null && !item.getChildItems().isEmpty()) {
                flattenItems(item.getChildItems(), gridData);
            }
        }
    }

    @PostMapping("/parse-treeview")
    public List<Map<String, Object>> parseForTreeView(@RequestBody String content) {
    try {

        ICb2XmlBuilder cb2= Cb2Xml3.newBuilder(new StringReader(content), "temp");
        cb2.setCobolLineFormat(Cb2xmlConstants.FREE_FORMAT);
        ICopybook copybook =cb2.asCobolItemTree();
        return buildTree(copybook.getChildItems());
    } catch (Exception e) {
        System.console().printf(e.getMessage());
        return Collections.emptyList();
    }
    }

    @PostMapping("/parse-treeviewgrid")
    public CopybookResponse parseForTreeViewGrid(@RequestBody String content) {
    try {

        ICb2XmlBuilder cb2= Cb2Xml3.newBuilder(new StringReader(content), "temp");
        cb2.setCobolLineFormat(Cb2xmlConstants.FREE_FORMAT);
        ICopybook copybook =cb2.asCobolItemTree();        
        return new CopybookResponse(true, buildTree(copybook.getChildItems()));
    } catch (Exception e) {
        System.console().printf(e.getMessage());
        return new CopybookResponse(false, "解析出錯: " + e.getMessage());
    }
    }    

    private List<Map<String, Object>> buildTree(List<? extends IItem> items) {
    List<Map<String, Object>> tree = new ArrayList<>();
    for (IItem item : items) {
        Map<String, Object> row = new LinkedHashMap<>();
        row.put("level", item.getLevelString());
        row.put("name", item.getFieldName());
        row.put("position", item.getPosition());
        row.put("length", item.getStorageLength());
        row.put("picture", item.getPicture());

        // 關鍵點：如果有子項目，遞迴放入 _children 欄位
        if (item.getChildItems() != null && !item.getChildItems().isEmpty()) {
            row.put("_children", buildTree(item.getChildItems()));
            }
            tree.add(row);
        }

        return tree;

    }

    // @PostMapping(value="/parse-treeViewGridInDetail", consumes = "application/json")
     @PostMapping(value="/parseInDepth")
    public CopybookResponse parseInDepth(@RequestBody String content) {
    try {
        // 1. 根據字串選擇對應的 CblLineFormat
            // Cb2xmlConstants.CblLineFormat format = Cb2xmlConstants CblLineFormat.valueOf(request.lineFormat.toUpperCase());

        ICb2XmlBuilder cb2= Cb2Xml3.newBuilder(new StringReader(content), "temp");
        cb2.setCobolLineFormat(Cb2xmlConstants.FREE_FORMAT);
        ICopybook copybook =cb2.asCobolItemTree();        
        return new CopybookResponse(true, buildTreeInDepthWithOccurs(copybook.getChildItems(),0,0,""));
    } catch (Exception e) {
        System.console().printf(e.getMessage());
        return new CopybookResponse(false, "解析出錯: " + e.getMessage());
    }
    }    

     @PostMapping(value="/parseInDepthStandardFormat")
    public CopybookResponse parseInDepthStandardFormat(@RequestBody String content) {
    try {
        // 1. 根據字串選擇對應的 CblLineFormat
            // Cb2xmlConstants.CblLineFormat format = Cb2xmlConstants CblLineFormat.valueOf(request.lineFormat.toUpperCase());

        ICb2XmlBuilder cb2= Cb2Xml3.newBuilder(new StringReader(content), "temp");
        cb2.setCobolLineFormat(Cb2xmlConstants.USE_STANDARD_COLUMNS);
        ICopybook copybook =cb2.asCobolItemTree();        
        return new CopybookResponse(true, buildTreeInDepthWithOccurs(copybook.getChildItems(),0,0,""));
    } catch (Exception e) {
        System.console().printf(e.getMessage());
        return new CopybookResponse(false, "解析出錯: " + e.getMessage());
    }
    }        

    private List<Map<String, Object>> buildTreeInDepthWithOccurs(List<? extends IItem> items, int depth, int parentOffsetShift, String parentIndexSuffix) 
    {
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
                    


                    // 關鍵點：如果有子項目，遞迴放入 _children 欄位
                    if (item.getChildItems() != null && !item.getChildItems().isEmpty()) {
                        row.put("_children", buildTreeInDepthWithOccurs(item.getChildItems(),depth+1,currentShift, currentIndexSuffix));
                        }
                        tree.add(row);
                }
        }

        return tree;

    }    
    
    private List<Map<String, Object>> buildTreeInDepth(List<? extends IItem> items, int depth) {
    List<Map<String, Object>> tree = new ArrayList<>();
    for (IItem item : items) {
        Map<String, Object> row = new LinkedHashMap<>();
        row.put("LevelString", item.getLevelString());
        row.put("LevelNumber", item.getLevelNumber());
        row.put("FieldName", item.getFieldName());
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
        row.put("Position", item.getPosition());        

        if (item.getOccurs() > 0){
            row.put("EndPosition", item.getPosition()+item.getStorageLength()*item.getOccurs()-1);
        }
        else{
            row.put("EndPosition", item.getPosition()+item.getStorageLength()-1);
        }
        
        row.put("RedefinesFieldName", item.getRedefinesFieldName());
        row.put("Scale", item.getScale());


        row.put("StorageLength", item.getStorageLength());

        if (item.getOccurs() > 0){
            row.put("TotalStorageLength", item.getStorageLength()*item.getOccurs());
        }
        else{
            row.put("TotalStorageLength", item.getStorageLength());
        }

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
        


        // 關鍵點：如果有子項目，遞迴放入 _children 欄位
        if (item.getChildItems() != null && !item.getChildItems().isEmpty()) {
            row.put("_children", buildTreeInDepth(item.getChildItems(),depth+1));
            }
            tree.add(row);
        }

        return tree;

    }    
   
     @PostMapping(value="/parseFlatten")
    public CopybookResponse parseFlatten(@RequestBody String content) {
    try {
        // 1. 根據字串選擇對應的 CblLineFormat
            // Cb2xmlConstants.CblLineFormat format = Cb2xmlConstants CblLineFormat.valueOf(request.lineFormat.toUpperCase());

        ICb2XmlBuilder cb2= Cb2Xml3.newBuilder(new StringReader(content), "temp");        

        cb2.setCobolLineFormat(Cb2xmlConstants.FREE_FORMAT);
        ICopybook copybook =cb2.asCobolItemTree();        

        List<Map<String, Object>> gridData = new ArrayList<>();
        buildTreeFlatten(copybook.getChildItems(),gridData,0);

        return new CopybookResponse(true, gridData);
    } catch (Exception e) {
        System.console().printf(e.getMessage());
        return new CopybookResponse(false, "解析出錯: " + e.getMessage());
    
    }
    }    

    private void buildTreeFlatten(List<? extends IItem> items,List<Map<String, Object>> gridData, int depth) {
    
    for (IItem item : items) {
        Map<String, Object> row = new LinkedHashMap<>();
        row.put("LevelString", item.getLevelString());
        row.put("LevelNumber", item.getLevelNumber());
        row.put("FieldName", item.getFieldName());
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
        row.put("Position", item.getPosition());        

        if (item.getOccurs() > 0){
            row.put("EndPosition", item.getPosition()+item.getStorageLength()*item.getOccurs()-1);
        }
        else{
            row.put("EndPosition", item.getPosition()+item.getStorageLength()-1);
        }
        
        row.put("RedefinesFieldName", item.getRedefinesFieldName());
        row.put("Scale", item.getScale());


        row.put("StorageLength", item.getStorageLength());

        if (item.getOccurs() > 0){
            row.put("TotalStorageLength", item.getStorageLength()*item.getOccurs());
        }
        else{
            row.put("TotalStorageLength", item.getStorageLength());
        }

        row.put("isSync", item.isSync());        
        

        row.put("Value", item.getValue());

        row.put("isFieldRedefined", item.isFieldRedefined());
        row.put("isFieldRedefines", item.isFieldRedefines());
        row.put("isInheritedUsage", item.isInheritedUsage());
        row.put("isBlankWhenZero", item.isBlankWhenZero());
        row.put("RelativeLevel", item.getRelativeLevel());
        row.put("Depth", depth);
        gridData.add(row);
        // 如果有子項目，遞迴處理
            if (item.getChildItems() != null && !item.getChildItems().isEmpty()) {
                buildTreeFlatten(item.getChildItems(), gridData,depth+1);
            }

    }    

    }

}
