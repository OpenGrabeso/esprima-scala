import {SourceLocation, Comment} from "./src/scanner";

export interface Node {
    type: String;
    range: number[];
    loc: SourceLocation;
    leadingComments: Comment[];
    innerComments:  Comment[];
    trailingComments: Comment[];
}
