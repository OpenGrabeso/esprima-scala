export * from "src/assert"
export * from "src/character"
export * from "src/comment-handler"
export * from "src/error-handler"
export * from "src/esprima"
//export * from "src/jsx-nodes"
//export * from "src/jsx-parser"
//export * from "src/jsx-syntax"
export * from "src/messages"
export * from "src/nodes"
export * from "src/parser"
export * from "src/scanner"
export * from "src/syntax"
export * from "src/token"
export * from "src/tokenizer"
export * from "src/xhtml-entities"
export * from "types"

var ScalaFromJS_settings = {
    members: [
        {
            cls: "^(?!Token$)(?!Syntax$).+$", // anything but Token or Syntax
            name: "type",
            operation: "getClass"
        }
    ],
    packages: [
        {
            folder: "test",
            operation: "name",
            name: "com.github.opengrabeso.esprima",
            template: [
                "class ${class}Tests extends Tests {",
                "import org.scalatest.Assertions.{assert => ok}",
                "",
                "$this",
                "}"
            ]
        },
        {
            folder: "src",
            operation: "name",
            name: "com.github.opengrabeso.esprima"
        },

    ],
    preprocess: [
        {
            operation: "replace",
            pattern: "import \\{ JSXParser \\} from '\\./jsx-parser';",
            replace: ""
        },
    ],

    postprocess: [
        {
            operation: "replace",
            pattern: "ok\\(false, \"everything",
            replace: "// ok(false, \"everything"
        },
        {
            operation: "replace",
            pattern: "module.*\\((.*),.*\\=\\>",
            replace: "module($1, implicit mod =>"
        },
        {
            operation: "replace",
            pattern: "import src\\.",
            replace: "import esprima."
        }
    ]
};