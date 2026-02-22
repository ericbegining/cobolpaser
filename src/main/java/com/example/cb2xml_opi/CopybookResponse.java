package com.example.cb2xml_opi;

public class CopybookResponse {
    public boolean success;
    public Object data;
    public String error;

    public CopybookResponse(boolean success, Object data) {
        this.success = success;
        this.data = data;
    }
    public CopybookResponse(boolean success, String error) {
        this.success = success;
        this.error = error;
    }
}
