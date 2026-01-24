# COBOL Copybook Analyzer

## 目的
    為減少手動計算 Cobol Copybook 結構長度的煩躁，故建立本網頁

## 功能
- 由網頁前端輸入 copybook 結構，解析出起始位置、欄位長度等資訊
- 後端可供程式介接，以產生需要的文件或程式碼
    - 如 : 撰寫csv報表時，需要的標頭、內容格式，可透過取得的copybook結構，依長度、類型逐欄建立

## Cb2xml
後端使用 Cb2xml 解析 Copybook
- 連結 : https://github.com/bmTas/cb2xml
- 授權 : LGPL-2.1 license