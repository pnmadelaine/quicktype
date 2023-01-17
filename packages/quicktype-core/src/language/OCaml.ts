import { TargetLanguage } from "../TargetLanguage";
import { ConvenienceRenderer, ForbiddenWordsInfo } from "../ConvenienceRenderer";
import {
    legalizeCharacters,
    splitIntoWords,
    isLetterOrUnderscoreOrDigit,
    combineWords,
    allLowerWordStyle,
    firstUpperWordStyle,
    isAscii,
    isLetterOrUnderscore
} from "../support/Strings";
import { Name, Namer, funPrefixNamer } from "../Naming";
import { UnionType, Type, ClassType, EnumType } from "../Type";
import { matchType, nullableFromUnion, removeNullFromUnion } from "../TypeUtils";
import { Sourcelike } from "../Source";
import { StringOption, getOptionValues } from "../RendererOptions";
import { RenderContext } from "../Renderer";

export const ocamlOptions = {
    deriving: new StringOption("deriving", "Deriving extensions to use", "list", ""),
};

export class OCamlTargetLanguage extends TargetLanguage {
    protected makeRenderer(renderContext: RenderContext, untypedOptionValues: { [name: string]: any }): OCamlRenderer {
        return new OCamlRenderer(this, renderContext, getOptionValues(ocamlOptions, untypedOptionValues));
    }

    constructor() {
        super("OCaml", ["ocaml"], "ml");
    }

    protected getOptions(): Option<any>[] {
        return [
            ocamlOptions.deriving
        ];
    }
}

const keywords = [
    "and",
    "as",
    "assert",
    "asr",
    "begin",
    "class",
    "constraint",
    "do",
    "done",
    "downto",
    "else",
    "end",
    "exception",
    "external",
    "false",
    "for",
    "fun",
    "function",
    "functor",
    "if",
    "in",
    "include",
    "inherit",
    "initializer",
    "land",
    "lazy",
    "let",
    "lor",
    "lsl",
    "lsr",
    "lxor",
    "match",
    "method",
    "mod",
    "module",
    "mutable",
    "new",
    "nonrec",
    "object",
    "of",
    "open",
    "or",
    "private",
    "rec",
    "sig",
    "struct",
    "then",
    "to",
    "true",
    "try",
    "type",
    "val",
    "virtual",
    "when",
    "while",
    "with",

    "None",
    "Some"
];

const isAsciiLetterOrUnderscoreOrDigit = (codePoint: number): boolean => {
    if (!isAscii(codePoint)) {
        return false;
    }

    return isLetterOrUnderscoreOrDigit(codePoint);
};

const isAsciiLetterOrUnderscore = (codePoint: number): boolean => {
    if (!isAscii(codePoint)) {
        return false;
    }

    return isLetterOrUnderscore(codePoint);
};

const legalizeName = legalizeCharacters(isAsciiLetterOrUnderscoreOrDigit);

function ocamlStyle(original: string, isSnakeCase: boolean): string {
    const words = splitIntoWords(original);

    const wordStyle = isSnakeCase ? allLowerWordStyle : firstUpperWordStyle;

    const combined = combineWords(
        words,
        legalizeName,
        wordStyle,
        wordStyle,
        wordStyle,
        wordStyle,
        isSnakeCase ? "_" : "",
        isAsciiLetterOrUnderscore
    );

    return combined === "_" ? "_underscore" : combined;
}

const snakeNamingFunction = funPrefixNamer("default", (original: string) => ocamlStyle(original, true));
const camelNamingFunction = funPrefixNamer("camel", (original: string) => ocamlStyle(original, false));

export class OCamlRenderer extends ConvenienceRenderer {
    constructor(
        targetLanguage: TargetLanguage,
        renderContext: RenderContext,
        private readonly _options: OptionValues<typeof ocamlOptions>
    ) {
        super(targetLanguage, renderContext);
    }

    private isFirstTypeDeclBool: bool = true;

    private typeDeclKeyword(): bool {
        let b = this.isFirstTypeDeclBool;
        this.isFirstTypeDeclBool = false;
        return b ? "type " : "and ";
    }

    private isFirstFunDeclBool: bool = true;

    private funDeclKeyword(): bool {
        let b = this.isFirstFunDeclBool;
        this.isFirstFunDeclBool = false;
        return b ? "let rec " : "and ";
    }

    protected makeNamedTypeNamer(): Namer {
        return snakeNamingFunction;
    }

    protected namerForObjectProperty(): Namer | null {
        return snakeNamingFunction;
    }

    protected makeUnionMemberNamer(): Namer | null {
        return camelNamingFunction;
    }

    protected makeEnumCaseNamer(): Namer | null {
        return camelNamingFunction;
    }

    protected forbiddenNamesForGlobalNamespace(): string[] {
        return keywords;
    }

    protected forbiddenForObjectProperties(_c: ClassType, _className: Name): ForbiddenWordsInfo {
        return { names: [], includeGlobalForbidden: true };
    }

    protected forbiddenForUnionMembers(_u: UnionType, _unionName: Name): ForbiddenWordsInfo {
        return { names: [], includeGlobalForbidden: true };
    }

    protected forbiddenForEnumCases(_e: EnumType, _enumName: Name): ForbiddenWordsInfo {
        return { names: [], includeGlobalForbidden: true };
    }

    protected get commentLineStart(): string {
        return "(* ";
    }

    protected get commentLineEnd(): string {
        return " *)";
    }

    private ocamlType(t: Type): Sourcelike {
        return matchType<Sourcelike>(
            t,
            _anyType => "Yojson.Safe.t",
            _nullType => "unit",
            _boolType => "bool",
            _integerType => "int",
            _doubleType => "float",
            _stringType => "string",
            arrayType => ["(", this.ocamlType(arrayType.items), ") list"],
            classType => this.nameForNamedType(classType),
            mapType => ["(string * ", this.ocamlType(mapType.values), ") list"],
            enumType => this.nameForNamedType(enumType),
            unionType => {
                const nullable = nullableFromUnion(unionType);

                if (nullable !== null) return ["(", this.ocamlType(nullable), ") option"];

                const [hasNull] = removeNullFromUnion(unionType);

                const name = this.nameForNamedType(unionType);

                return hasNull !== null ? (["(", name, ") option"] as Sourcelike) : name;
            }
        );
    }

    private emitDeriving(): void {
        if (this._options.deriving !== "") {
            this.emitLine(["[@@deriving ", this._options.deriving, "]"]);
        }
    }

    private parserName(name: Name): SourceLike {
        return ["parse_", name];
    }

    private serializerName(name: Name): SourceLike {
        return ["serialize_", name];
    }

    protected inlineParserAux(match: SourceLike): SourceLike {
        return ["(fun j -> match j with ", match, " | _ -> raise InvalidJson)"]
    }

    protected inlineParser(t:Type): SourceLike {
        return matchType<Sourcelike>(
            t,
            _anyType => this.inlineParserAux("x -> x"),
            _nullType => this.inlineParserAux("`Null -> ()"),
            _boolType => this.inlineParserAux("`Bool b -> b"),
            _integerType => this.inlineParserAux("`Int i -> i"),
            _doubleType => this.inlineParserAux("`Float f -> f | `Int i -> float_of_int i"),
            _stringType => this.inlineParserAux("`String s -> s"),
            arrayType => this.inlineParserAux(["`List l -> List.map ", this.inlineParser(arrayType.items), " l"]),
            classType => this.parserName(this.nameForNamedType(classType)),
            mapType => this.inlineParserAux(["| `Assoc l -> List.map (fun (name, x) -> (name, ", this.inlineParser(mapType.values), " x)) l"]),
            enumType => this.parserName(this.nameForNamedType(enumType)),
            unionType =>  {
                const nullable = nullableFromUnion(unionType);

                if (nullable !== null) return this.inlineParserAux(["`Null -> None | x -> Some (", this.inlineParser(nullable), " x)"]);

                const [hasNull] = removeNullFromUnion(unionType);

                const name = this.parserName(this.nameForNamedType(unionType));

                return hasNull !== null ? this.inlineParserAux(["`Null -> None | x -> Some (", name, " x)"]) : name;
            }
        );
    }

    protected inlineSerializer(t:Type): SourceLike {
        return matchType<Sourcelike>(
            t,
            _anyType => "(fun x -> x)",
            _nullType => "(fun _ -> `Null)",
            _boolType => "(fun b -> `Bool b)",
            _integerType => "(fun i -> `Int i)",
            _doubleType => "(fun f -> `Float f)",
            _stringType => "(fun s -> `String s)",
            arrayType => ["(fun x -> `List (List.map ", this.inlineSerializer(arrayType.items), " x))"],
            classType => this.serializerName(this.nameForNamedType(classType)),
            mapType => ["(fun l -> `Assoc (List.map (fun (name, x) -> (name, ", this.inlineSerializer(mapType.values), " x)) l))"],
            enumType => this.serializerName(this.nameForNamedType(enumType)),
            unionType =>  {
                const nullable = nullableFromUnion(unionType);

                if (nullable !== null) return ["(fun x -> match x with None -> `Null | Some x -> ", this.inlineSerializer(nullable), " x)"];

                const [hasNull] = removeNullFromUnion(unionType);

                const name = this.serializerName(this.nameForNamedType(unionType));

                return hasNull !== null ? ["(fun x -> match x with None -> `Null | Some x -> ", name, " x)"] : name;
            }
        );
    }

    protected emitStructDefinition(c: ClassType, className: Name): void {
        this.emitDescription(this.descriptionForType(c));

        this.emitLine([this.typeDeclKeyword(), className, " = {"]);
        this.indent(() =>
            this.forEachClassProperty(c, "none", (name, jsonName, prop) => {
                this.emitDescription(this.descriptionForClassProperty(c, jsonName));
                this.emitLine(name, ": ", this.ocamlType(prop.type), ";");
            })
        );
        this.emitLine("}");

        this.emitDeriving();
    }

    private emitStructParser(c: ClassType, className: Name): void {
        this.emitLine([this.funDeclKeyword(), this.parserName(className), " (json:Yojson.Safe.t): ", className, " ="]);
        this.emitLine(["  match json with"]);
        this.emitLine(["  | `Assoc l -> {"]);
        this.forEachClassProperty(c, "none", (name, jsonName, prop) => {
            this.emitLine(["    ", name, " = ", this.inlineParser(prop.type), " (try List.assoc \"", jsonName, "\" l with _ -> `Null);"]);
        });
        this.emitLine(["  }"]);
        this.emitLine(["  | _ -> raise InvalidJson"]);
    }

    private emitStructSerializer(c: ClassType, className: Name): void {
        this.emitLine([this.funDeclKeyword(), this.serializerName(className), " (x: ", className, "): Yojson.Safe.t ="]);
        this.emitLine("`Assoc [");
        this.forEachClassProperty(c, "none", (name, jsonName, prop) => {
            this.emitLine(["    (\"", jsonName, "\", ", this.inlineSerializer(prop.type), " x.", name, ");"]);
        });
        this.emitLine(["]"]);
    }

    protected emitUnionDefinition(u: UnionType, unionName: Name): void {
        const isMaybeWithSingleType = nullableFromUnion(u);

        if (isMaybeWithSingleType !== null) {
            return;
        }

        this.emitDescription(this.descriptionForType(u));

        const [, nonNulls] = removeNullFromUnion(u);

        this.emitLine([this.typeDeclKeyword(), unionName, " ="]);
        this.forEachUnionMember(u, nonNulls, "none", null, (fieldName, t) => {
            const ocamlType = this.ocamlType(t, true);
            this.emitLine(["| ", fieldName, " of ", ocamlType]);
        });
        this.emitDeriving();
    }

    protected emitUnionParser(u: unionType, unionName: Name): void {
        this.emitLine([this.funDeclKeyword(), this.parserName(unionName), " (json:Yojson.Safe.t): ", unionName, " ="]);

        const [, nonNulls] = removeNullFromUnion(u);

        this.forEachUnionMember(u, nonNulls, "none", null, (fieldName, t) => {
            this.emitLine(["  try ", fieldName, " (", this.inlineParser(t), " json) with _ ->"]);
        });
        this.emitLine(["  raise InvalidJson"]);
    }

    protected emitUnionSerializer(u: unionType, unionName: Name): void {
        this.emitLine([this.funDeclKeyword(), this.serializerName(unionName), " (x: ", unionName, "): Yojson.Safe.t ="]);

        const [, nonNulls] = removeNullFromUnion(u);

        this.emitLine(["  match x with"]);
        this.forEachUnionMember(u, nonNulls, "none", null, (fieldName, t) => {
            this.emitLine(["  | ", fieldName, " x -> ", this.inlineSerializer(t), " x"]);
        });
    }

    protected emitEnumDefinition(e: EnumType, enumName: Name): void {
        this.emitDescription(this.descriptionForType(e));

        this.emitLine([this.typeDeclKeyword(), enumName, " ="]);
        this.forEachEnumCase(e, "none", (name, jsonName) => {
            this.emitLine(["| ", name]);
        });
        this.emitDeriving();
    }

    protected emitEnumParser(e: EnumType, enumName: Name): void {
        this.emitLine([this.funDeclKeyword(), this.parserName(enumName), " (json:Yojson.Safe.t): ", enumName, " ="]);
        this.emitLine(["  match json with"]);
        this.forEachEnumCase(e, "none", (name, jsonName) => {
            this.emitLine(["  | `String \"", jsonName, "\" -> ", name]);
        });
        this.emitLine(["  | _ -> raise InvalidJson"]);
    }

    protected emitEnumSerializer(e: EnumType, enumName: Name): void {
        this.emitLine([this.funDeclKeyword(), this.serializerName(enumName), " (x: ", enumName, "): Yojson.Safe.t ="]);
        this.emitLine(["  match x with"]);
        this.forEachEnumCase(e, "none", (name, jsonName) => {
            this.emitLine(["  | ", name, " -> `String \"", jsonName, "\""]);
        });
    }

    protected emitTopLevelAlias(t: Type, name: Name): void {
        this.emitLine(this.typeDeclKeyword(), name, " = ", this.ocamlType(t));
        this.emitDeriving();
    }

    protected emitSourceStructure(): void {
        if (this.leadingComments !== undefined) {
            this.emitCommentLines(this.leadingComments);
            return;
        }

        this.emitLine("open Yojson");
        this.ensureBlankLine();
        this.emitLine("exception InvalidJson");
        this.ensureBlankLine();

        this.forEachTopLevel(
            "leading",
            (t, name) => this.emitTopLevelAlias(t, name),
            t => this.namedTypeToNameForTopLevel(t) === undefined
        );

        this.forEachObject("leading-and-interposing", (c, name) => this.emitStructDefinition(c, name));
        this.forEachUnion("leading-and-interposing", (u, name) => this.emitUnionDefinition(u, name));
        this.forEachEnum("leading-and-interposing", (e, name) => this.emitEnumDefinition(e, name));

        this.forEachObject("leading-and-interposing", (c, name) => this.emitStructParser(c, name));
        this.forEachUnion("leading-and-interposing", (u, name) => this.emitUnionParser(u, name));
        this.forEachEnum("leading-and-interposing", (e, name) => this.emitEnumParser(e, name));

        this.forEachObject("leading-and-interposing", (c, name) => this.emitStructSerializer(c, name));
        this.forEachUnion("leading-and-interposing", (u, name) => this.emitUnionSerializer(u, name));
        this.forEachEnum("leading-and-interposing", (e, name) => this.emitEnumSerializer(e, name));
    }
}
