# 使用 Python 連接 CopyBook 解析 API 範例

本文件說明如何使用 Python 呼叫 COBOL 解析 API (`/api/parseInDepth`) 並處理回傳的資料結構。

## 1. 環境需求
* **Python 3.x**
* **Requests 函式庫** 

## 2. Python 範例程式碼
```python
import requests
import json

# 1. 設定 API 位址：使用您指定的 Port 9000 與路徑
url = "http://localhost:9000/api/parseInDepth"

# 2. 準備測試資料：使用您提供的 REC-FACTURE 結構
cobol_code = """
       01  REC-FACTURE.
           03  FS1                 PIC X.
           03  FS2.
               05  FS2A            PIC 9.
               05  RFS2B           PIC X(8).
               05  FS2B REDEFINES RFS2B  PIC 9(8).
"""

def list_item_details(items):
    """
    遞迴函式：遍歷樹狀結構並列印所有 IItem 欄位資訊
    """
    for item in items:
        # 直接從字典取得欄位，並在旁邊標註 cb2xml IItem 的定義註解
        level_str = item.get("LevelString", "") # 欄位層級字串 (如 "01", "03")
        level_num = item.get("LevelNumber", 0)  # 欄位層級數值
        name = item.get("FieldName", "")        # 欄位名稱
        pic = item.get("Picture", "")           # PICTURE 格式遮罩
        usage = item.get("Usage", "")           # 儲存格式 (USAGE)，如 COMP-3
        pos = item.get("Position", 0)           # 紀錄中的起始位元組位置
        end_pos = item.get("EndPosition", 0)    # 紀錄中的結束位元組位置
        storage_len = item.get("StorageLength", 0) # 單一項目的儲存長度
        total_len = item.get("TotalStorageLength", 0) # 包含重複次數後的總長度
        occurs = item.get("Occurs", -1)         # 重複次數 (OCCURS)
                                                # values : {>0 :occurs 次數 ; -1:沒有 occurs}
        occurs_min = item.get("OccursMin", -1)  # 最小重複次數
                                                # {>0 :OccursMin 次數 ; -1:沒有 OccursMin}
        redefines = item.get("RedefinesFieldName", "") # 被重定義的欄位名稱
        num_class = item.get("NumericClass", 0) # 數值類別代碼
                                                # values : { NON_NUMERIC ; Numeric_Edited ; COBOL_NUMERIC}
                                                # Numeric_Edited : Numeric edited fields (e.g. -,---,--9.99 are not strictly numeric in Cobol but are often used to send numeric values to non Cobol Systems. So you should consider these fields as numeric
                                                
        scale = item.get("Scale", 0)            # 小數位數
        disp_len = item.get("DisplayLength", 0) # 顯示長度
        disp_pos = item.get("DisplayPosition", 0) # 顯示位置
        justified = item.get("Justified", "")   # 是否對齊 (JUSTIFIED)
        is_sync = item.get("isSync", False)     # 是否同步 (SYNCHRONIZED)
        value = item.get("Value", "")           # 初始值或 88 層級的數值
        dep_on = item.get("DependingOn", "")    # 依賴欄位 (DEPENDING ON)            
        is_redefined = item.get("isFieldRedefined", False) # 此欄位是否「被」其他欄位重定義
        is_redefines = item.get("isFieldRedefines", False) # 此欄位是否「正在」重定義其他欄位
        depth = item.get("Depth", 0)                       # 結構樹的深度 (0 為最頂層)
        is_inherited_usage = item.get("isInheritedUsage", False) # 是否繼承了父層的 USAGE 設定
        is_blank_when_zero = item.get("isBlankWhenZero", False)  # 是否設定為零時顯示空白
        rel_level = item.get("RelativeLevel", 0)           # 相對層級深度
        sign_clause= item.get("SignClause", 0)  # SignClause
                                                #  values : { NO_SIGN_CLAUSE ; <COBOL SIGN CLAUSE> }

        # 輸出結果：展示階層與關鍵資訊
        indent = "  " * depth
        print(f"{indent}[{level_str}] {name} (Pos: {pos}-{end_pos}, Len: {total_len})")
        print(f"{indent}    > Depth: {depth}, Redefines: {redefines}, Redefined: {is_redefined}")
        print(f"{indent}    > Usage: {usage}, Picture: {pic}, InheritedUsage: {is_inherited_usage}")

        # 如果有子項目，遞迴列舉 _children
        children = item.get("_children")
        if children:
            list_item_details(children)

# 3. 執行主程式
def main():
    try:
        # 發送 POST 請求，內容為純文字 COBOL 代碼
        print(f"發送 POST 請求至 {url}...")
        response = requests.post(url, data=cobol_code, headers={'Content-Type': 'text/plain'})
        
        # 4. 解析回應
        response.raise_for_status()
        res_json = response.json()
        
        if res_json.get("success"):
            print("解析成功，列舉欄位清單：\n")
            list_item_details(res_json.get("data", []))
        else:
            print(f"解析失敗：{res_json.get('error')}")
            
    except Exception as e:
        print(f"發生錯誤：{e}")

if __name__ == "__main__":
    main()

```

### 每個步驟的簡短說明

1. **設定 API Endpoing**：將 URL 指向 Spring Boot 伺服器的 `9000` Port 與 `/api/parseInDepth` 路徑。
2. **準備 COBOL Copybook**：定義測試用的 Copybook 字串。這將作為 Request Body 發送。
3. **發送請求**：使用 `POST` 方法並設定 `Content-Type: text/plain`。這是因為後端使用 `@RequestBody String` 接收資料。
4. **檢查回應狀態**：透過 `raise_for_status()` 確保 HTTP 連線成功（200 OK）。
5. **解構 JSON 資料**：解析回傳的 `CopybookResponse`，並存取內部的 `data` 樹狀物件。



