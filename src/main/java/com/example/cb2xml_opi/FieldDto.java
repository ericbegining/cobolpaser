package com.example.cb2xml_opi;

import java.util.ArrayList;
import java.util.List;

public class FieldDto {
    public String level;
    public String name;
    public int position;
    public int singleLength;
    public int totalLength;
    public String picture;
    public String redefines;
    public int occurs;
    public boolean isGap;
    public List<FieldDto> children = new ArrayList<>();

    // 構造函數方便快速建立物件
    public FieldDto(String level, String name, int position, int length) {
        this.level = level;
        this.name = name;
        this.position = position;
        this.singleLength = length;
        this.totalLength = length;
    }
}
