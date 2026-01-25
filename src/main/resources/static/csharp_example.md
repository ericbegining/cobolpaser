# 使用 C# 連接 CopyBook 解析 API 範例

本文件說明如何使用 C# 呼叫 COBOL 解析 API (`/api/parseInDepth`) 並處理回傳的資料結構。

## 1. 環境需求
* **.NET SDK (建議 .NET 6 以上版本)**
* **System.Text.Json** 

## 2. C# 範例程式碼
```C#
using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading.Tasks;

class Program
{
    // 1. 設定 API 位址：使用您指定的 Port 9000 與路徑
    private static readonly string Url = "http://localhost:9000/api/parseInDepth";

    // 2. 準備測試資料：使用您提供的 REC-FACTURE 結構
    private static readonly string CobolCode = @"
        01  REC-FACTURE.
            03  FS1                 PIC X.
            03  FS2.
                05  FS2A            PIC 9.
                05  RFS2B           PIC X(8).
                05  FS2B REDEFINES RFS2B  PIC 9(8).
";

    static async Task Main(string[] args)
    {
        try
        {
            // 3. 執行主程式
            Console.WriteLine($"發送 POST 請求至 {Url}...");

            using var client = new HttpClient();
            // 發送 POST 請求，內容為純文字 COBOL 代碼 (Content-Type: text/plain)
            var content = new StringContent(CobolCode, Encoding.UTF8, "text/plain");
            var response = await client.PostAsync(Url, content);

            // 4. 解析回應
            response.EnsureSuccessStatusCode(); // 確保 HTTP 連線成功 (200 OK)
            string jsonString = await response.Content.ReadAsStringAsync();

            var options = new JsonSerializerOptions { PropertyNameCaseInsensitive = true };
            var resJson = JsonSerializer.Deserialize<ApiResponse>(jsonString, options);

            if (resJson != null && resJson.Success)
            {
                Console.WriteLine("解析成功，列舉欄位清單：\n");
                ListItemDetails(resJson.Data);
            }
            else
            {
                Console.WriteLine($"解析失敗：{resJson?.Error}");
            }
        }
        catch (Exception e)
        {
            Console.WriteLine($"發生錯誤：{e.Message}");
        }
    }

    /// <summary>
    /// 遞迴函式：遍歷樹狀結構並列印所有 IItem 欄位資訊
    /// </summary>
    static void ListItemDetails(List<CopybookItem> items, int currentDepth = 0)
    {
        if (items == null) return;

        foreach (var item in items)
        {
            // 輸出結果：展示階層與關鍵資訊
            string indent = new string(' ', item.Depth * 2);
            Console.WriteLine($"{indent}[{item.LevelString}] {item.FieldName} (Pos: {item.Position}-{item.EndPosition}, Len: {item.TotalStorageLength})");
            Console.WriteLine($"{indent}    > Depth: {item.Depth}, Redefines: {item.RedefinesFieldName}, Redefined: {item.IsFieldRedefined}");
            Console.WriteLine($"{indent}    > Usage: {item.Usage}, Picture: {item.Picture}, InheritedUsage: {item.IsInheritedUsage}");

            // 如果有子項目，遞迴列舉 _children
            if (item.Children != null && item.Children.Count > 0)
            {
                ListItemDetails(item.Children);
            }
        }
    }
}

// --- 資料結構定義 (保留原始註釋) ---

public class ApiResponse
{
    public bool Success { get; set; }
    public string Error { get; set; }
    public List<CopybookItem> Data { get; set; }
}

public class CopybookItem
{
    public string LevelString { get; set; }        // 欄位層級字串 (如 "01", "03")
    public int LevelNumber { get; set; }           // 欄位層級數值
    public string FieldName { get; set; }          // 欄位名稱
    public string Picture { get; set; }            // PICTURE 格式遮罩
    public string Usage { get; set; }              // 儲存格式 (USAGE)，如 COMP-3
    public int Position { get; set; }              // 紀錄中的起始位元組位置
    public int EndPosition { get; set; }           // 紀錄中的結束位元組位置
    public int StorageLength { get; set; }         // 單一項目的儲存長度
    public int TotalStorageLength { get; set; }    // 包含重複次數後的總長度
    
    /// <summary>
    /// 重複次數 (OCCURS) values : {>0 :occurs 次數 ; -1:沒有 occurs}
    /// </summary>
    public int Occurs { get; set; }
    
    /// <summary>
    /// 最小重複次數 {>0 :OccursMin 次數 ; -1:沒有 OccursMin}
    /// </summary>
    public int OccursMin { get; set; }
    
    public string RedefinesFieldName { get; set; } // 被重定義的欄位名稱
    
    /// <summary>
    /// 數值類別代碼 values : { NON_NUMERIC ; Numeric_Edited ; COBOL_NUMERIC}
    /// Numeric_Edited : Numeric edited fields (e.g. -,---,--9.99 are not strictly numeric in Cobol but are often used to send numeric values to non Cobol Systems. So you should consider these fields as numeric
    /// </summary>
    public string NumericClass { get; set; }
    
    public int Scale { get; set; }                 // 小數位數
    public int DisplayLength { get; set; }         // 顯示長度
    public int DisplayPosition { get; set; }      // 顯示位置
    public string Justified { get; set; }          // 是否對齊 (JUSTIFIED)
    
    [JsonPropertyName("isSync")]
    public bool IsSync { get; set; }               // 是否同步 (SYNCHRONIZED)
    
    public string Value { get; set; }              // 初始值或 88 層級的數值
    public string DependingOn { get; set; }        // 依賴欄位 (DEPENDING ON)
    
    [JsonPropertyName("isFieldRedefined")]
    public bool IsFieldRedefined { get; set; }     // 此欄位是否「被」其他欄位重定義
    
    [JsonPropertyName("isFieldRedefines")]
    public bool IsFieldRedefines { get; set; }     // 此欄位是否「正在」重定義其他欄位
    
    public int Depth { get; set; }                 // 結構樹的深度 (0 為最頂層)
    
    [JsonPropertyName("isInheritedUsage")]
    public bool IsInheritedUsage { get; set; }     // 是否繼承了父層標記的 USAGE 設定
    
    [JsonPropertyName("isBlankWhenZero")]
    public bool IsBlankWhenZero { get; set; }      // 是否設定為零時顯示空白
    
    public int RelativeLevel { get; set; }         // 相對層級深度
    
    /// <summary>
    /// SignClause values : { NO_SIGN_CLAUSE ; <COBOL SIGN CLAUSE> }
    /// </summary>
    public int SignClause { get; set; }

    [JsonPropertyName("_children")]
    public List<CopybookItem> Children { get; set; }
}

```

### 每個步驟的簡短說明

1. **設定 API Endpoing**：將 URL 指向 Spring Boot 伺服器的 `9000` Port 與 `/api/parseInDepth` 路徑。
2. **準備 COBOL Copybook**：定義測試用的 Copybook 字串。這將作為 Request Body 發送。
3. **發送請求**：使用 `POST` 方法並設定 `Content-Type: text/plain`。這是因為後端使用 `@RequestBody String` 接收資料。
4. **檢查回應狀態**：透過 `raise_for_status()` 確保 HTTP 連線成功（200 OK）。
5. **解構 JSON 資料**：解析回傳的 `CopybookResponse`，並存取內部的 `data` 樹狀物件。



