// Introduce types which can serve as interfaces

import {SourceLocation} from "./src/scanner";
import {Token} from "./src/token";

class Node {
    constructor() {
        this.type = "";
        this.range = [0, 0];
        this.loc = new SourceLocation();
        this.leadingComments = [];
        this.innerComments = [];
        this.trailingComments = [];
    }
}


class Position {
    constructor() {
        this.line = 0;
        this.column = 0;
    }
}

class SourceLocation {
    constructor() {
        this.start = new Position;
        this.end = new Position;
        this.source = ""
    }
}

class Comment {
    constructor() {
        this.multiLine = false;
        this.slice = [0, 0];
        this.range = [0, 0];
        this.loc = new SourceLocation;
    }
}

class RawToken {
    constructor() {
        this.type = 0;
        this.value = undefined;
        this.pattern = "";
        this.flags = "";
        this.regex = /.*/g;
        this.octal = false;
        this.cooked = "";
        this.head = false;
        this.tail = false;
        this.lineNumber = 0;
        this.lineStart = 0;
        this.start = 0;
        this.end = 0
    }
}

class ScannerState {
    constructor() {
        this.index = 0;
        this.lineNumber = 0;
        this.lineStart = 0
    }
}
