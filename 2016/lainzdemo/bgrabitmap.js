var Rectangle = (function () {
    function Rectangle(left, top, right, bottom) {
        this.raw = new Int32Array(4);
        this.Left = left;
        this.Top = top;
        this.Right = right;
        this.Bottom = bottom;
    }
    Object.defineProperty(Rectangle.prototype, "Left", {
        get: function () {
            return this.raw[0];
        },
        set: function (v) {
            this.raw[0] = Math.floor(v);
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Rectangle.prototype, "Top", {
        get: function () {
            return this.raw[1];
        },
        set: function (v) {
            this.raw[1] = Math.floor(v);
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Rectangle.prototype, "Right", {
        get: function () {
            return this.raw[2];
        },
        set: function (v) {
            this.raw[2] = Math.floor(v);
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Rectangle.prototype, "Bottom", {
        get: function () {
            return this.raw[3];
        },
        set: function (v) {
            this.raw[3] = Math.floor(v);
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(Rectangle.prototype, "Empty", {
        get: function () {
            return (this.Left == this.Right && this.Top == this.Bottom);
        },
        enumerable: true,
        configurable: true
    });
    Rectangle.prototype.Contains = function (x, y) {
        return x >= this.Left && y >= this.Top && x < this.Right && y < this.Bottom;
    };
    Rectangle.prototype.toString = function () {
        return "Rect(" + this.Left + "," + this.Top + "," + this.Right + "," + this.Bottom + ")";
    };
    Rectangle.prototype.Clone = function () {
        return new Rectangle(this.Left, this.Top, this.Right, this.Bottom);
    };
    Rectangle.prototype.Intersect = function (other) {
        if (other.Left > this.Right ||
            other.Right < this.Left ||
            other.Top > this.Bottom ||
            other.Bottom < this.Top) {
            this.Left = 0;
            this.Right = 0;
            this.Top = 0;
            this.Bottom = 0;
            return false;
        }
        else {
            if (other.Left > this.Left)
                this.Left = other.Left;
            if (other.Top > this.Top)
                this.Top = other.Top;
            if (other.Right < this.Right)
                this.Right = other.Right;
            if (other.Bottom < this.Bottom)
                this.Bottom = other.Bottom;
            return true;
        }
    };
    Rectangle.prototype.Equals = function (other) {
        return other === this || (other.Empty && this.Empty) ||
            (other.Left == this.Left && other.Top == this.Top && other.Right == this.Right && other.Bottom == this.Bottom);
    };
    return Rectangle;
}());
function Rect(left, top, right, bottom) {
    return new Rectangle(left, top, right, bottom);
}
var Size = (function () {
    function Size(cx, cy) {
        this.cx = cx;
        this.cy = cy;
    }
    return Size;
}());
var Point = (function () {
    function Point(x, y) {
        this.x = x;
        this.y = y;
    }
    return Point;
}());
var Alignment;
(function (Alignment) {
    Alignment[Alignment["LeftJustify"] = 0] = "LeftJustify";
    Alignment[Alignment["RightJustify"] = 1] = "RightJustify";
    Alignment[Alignment["Center"] = 2] = "Center";
})(Alignment || (Alignment = {}));
;
var TextLayout;
(function (TextLayout) {
    TextLayout[TextLayout["Top"] = 0] = "Top";
    TextLayout[TextLayout["Center"] = 1] = "Center";
    TextLayout[TextLayout["Bottom"] = 2] = "Bottom";
})(TextLayout || (TextLayout = {}));
;
var TextStyle = (function () {
    function TextStyle() {
    }
    return TextStyle;
}());
var FloodfillMode;
(function (FloodfillMode) {
    FloodfillMode[FloodfillMode["Set"] = 0] = "Set";
    FloodfillMode[FloodfillMode["DrawWithTransparency"] = 1] = "DrawWithTransparency";
    FloodfillMode[FloodfillMode["Progressive"] = 2] = "Progressive";
})(FloodfillMode || (FloodfillMode = {}));
;
var MedianOption;
(function (MedianOption) {
    MedianOption[MedianOption["None"] = 0] = "None";
    MedianOption[MedianOption["LowSmooth"] = 1] = "LowSmooth";
    MedianOption[MedianOption["MediumSmooth"] = 2] = "MediumSmooth";
    MedianOption[MedianOption["HighSmooth"] = 3] = "HighSmooth";
})(MedianOption || (MedianOption = {}));
;
var RadialBlurType;
(function (RadialBlurType) {
    RadialBlurType[RadialBlurType["Normal"] = 0] = "Normal";
    RadialBlurType[RadialBlurType["Disk"] = 1] = "Disk";
    RadialBlurType[RadialBlurType["Corona"] = 2] = "Corona";
    RadialBlurType[RadialBlurType["Precise"] = 3] = "Precise";
    RadialBlurType[RadialBlurType["Fast"] = 4] = "Fast";
    RadialBlurType[RadialBlurType["Box"] = 5] = "Box";
})(RadialBlurType || (RadialBlurType = {}));
;
var EmbossOption;
(function (EmbossOption) {
    EmbossOption[EmbossOption["Transparent"] = 0] = "Transparent";
    EmbossOption[EmbossOption["PreserveHue"] = 1] = "PreserveHue";
})(EmbossOption || (EmbossOption = {}));
;
function CheckPutImageBounds(x, y, tx, ty, minxb, minyb, maxxb, maxyb, ignoreleft, cliprect) {
    return false;
}
var RawImageLineOrder;
(function (RawImageLineOrder) {
    RawImageLineOrder[RawImageLineOrder["TopToBottom"] = 0] = "TopToBottom";
    RawImageLineOrder[RawImageLineOrder["BottomToTop"] = 1] = "BottomToTop";
})(RawImageLineOrder || (RawImageLineOrder = {}));
;
var RawImageBitOrder;
(function (RawImageBitOrder) {
    RawImageBitOrder[RawImageBitOrder["BitsInOrder"] = 0] = "BitsInOrder";
    RawImageBitOrder[RawImageBitOrder["ReversedBits"] = 1] = "ReversedBits";
})(RawImageBitOrder || (RawImageBitOrder = {}));
;
var RawImageByteOrder;
(function (RawImageByteOrder) {
    RawImageByteOrder[RawImageByteOrder["LSBFirst"] = 0] = "LSBFirst";
    RawImageByteOrder[RawImageByteOrder["MSBFirst"] = 1] = "MSBFirst";
})(RawImageByteOrder || (RawImageByteOrder = {}));
;
var GraphicsBevelCut;
(function (GraphicsBevelCut) {
    GraphicsBevelCut[GraphicsBevelCut["None"] = 0] = "None";
    GraphicsBevelCut[GraphicsBevelCut["Lowered"] = 1] = "Lowered";
    GraphicsBevelCut[GraphicsBevelCut["Raised"] = 2] = "Raised";
    GraphicsBevelCut[GraphicsBevelCut["Space"] = 3] = "Space";
})(GraphicsBevelCut || (GraphicsBevelCut = {}));
;
var GraphicsFillStyle;
(function (GraphicsFillStyle) {
    GraphicsFillStyle[GraphicsFillStyle["Surface"] = 0] = "Surface";
    GraphicsFillStyle[GraphicsFillStyle["Border"] = 1] = "Border";
})(GraphicsFillStyle || (GraphicsFillStyle = {}));
;
function PositiveMod(value, cycle) {
    var result;
    result = value % cycle;
    if (result < 0) {
        result += cycle;
    }
    return result;
}
var sinTab65536;
var byteSqrtTab;
function Sin65536(value) {
    var b;
    var result;
    if (sinTab65536 == null) {
        sinTab65536 = new Uint16Array(32768);
    }
    if (value >= 32768) {
        b = value ^ 32768;
        if (sinTab65536[b] = 0) {
            sinTab65536[b] = Math.round((Math.sin(b * 2 * Math.PI / 65536) + 1) * 65536 / 2) - 1;
        }
        result = ~sinTab65536[b];
    }
    else {
        b = value;
        if (sinTab65536[b] = 0) {
            sinTab65536[b] = Math.round((Math.sin(b * 2 * Math.PI / 65536) + 1) * 65536 / 2) - 1;
        }
        result = sinTab65536[b] + 1;
    }
    return result;
}
function Cos65536(value) {
    return Sin65536(value + 16384);
}
function PrecalcSin65536() {
    var i;
    for (i = 0; i <= 32767; i++) {
        Sin65536(i);
    }
}
function PrecalcByteSqrt() {
    var i;
    if (byteSqrtTab == null) {
        byteSqrtTab = new Uint16Array(256);
        for (i = 0; i <= 255; i++) {
            byteSqrtTab[i] = Math.round(Math.sqrt(i / 255) * 255);
        }
    }
}
function ByteSqrt(value) {
    if (byteSqrtTab == null) {
        PrecalcByteSqrt;
    }
    return byteSqrtTab[value];
}
var BGRAFontQuality;
(function (BGRAFontQuality) {
    BGRAFontQuality[BGRAFontQuality["System"] = 0] = "System";
    BGRAFontQuality[BGRAFontQuality["SytemClearType"] = 1] = "SytemClearType";
    BGRAFontQuality[BGRAFontQuality["FineAntialiasing"] = 2] = "FineAntialiasing";
    BGRAFontQuality[BGRAFontQuality["FineClearTypeRGB"] = 3] = "FineClearTypeRGB";
    BGRAFontQuality[BGRAFontQuality["FineClearTypeBGR"] = 4] = "FineClearTypeBGR";
})(BGRAFontQuality || (BGRAFontQuality = {}));
;
var FontPixelMetric = (function () {
    function FontPixelMetric(defined, baseline, xline, capline, descentline, lineheight) {
        this.Defined = defined;
        this.Baseline = baseline;
        this.xLine = xline;
        this.CapLine = capline;
        this.DescentLine = descentline;
        this.Lineheight = lineheight;
    }
    return FontPixelMetric;
}());
var FontVerticalAnchor;
(function (FontVerticalAnchor) {
    FontVerticalAnchor[FontVerticalAnchor["Top"] = 0] = "Top";
    FontVerticalAnchor[FontVerticalAnchor["Center"] = 1] = "Center";
    FontVerticalAnchor[FontVerticalAnchor["CapLine"] = 2] = "CapLine";
    FontVerticalAnchor[FontVerticalAnchor["CapCenter"] = 3] = "CapCenter";
    FontVerticalAnchor[FontVerticalAnchor["XLine"] = 4] = "XLine";
    FontVerticalAnchor[FontVerticalAnchor["XCenter"] = 5] = "XCenter";
    FontVerticalAnchor[FontVerticalAnchor["Baseline"] = 6] = "Baseline";
    FontVerticalAnchor[FontVerticalAnchor["DescentLine"] = 7] = "DescentLine";
    FontVerticalAnchor[FontVerticalAnchor["Bottom"] = 8] = "Bottom";
})(FontVerticalAnchor || (FontVerticalAnchor = {}));
;
var BGRATypeWriterAlignment;
(function (BGRATypeWriterAlignment) {
    BGRATypeWriterAlignment[BGRATypeWriterAlignment["TopLeft"] = 0] = "TopLeft";
    BGRATypeWriterAlignment[BGRATypeWriterAlignment["Top"] = 1] = "Top";
    BGRATypeWriterAlignment[BGRATypeWriterAlignment["TopRight"] = 2] = "TopRight";
    BGRATypeWriterAlignment[BGRATypeWriterAlignment["Left"] = 3] = "Left";
    BGRATypeWriterAlignment[BGRATypeWriterAlignment["Middle"] = 4] = "Middle";
    BGRATypeWriterAlignment[BGRATypeWriterAlignment["Right"] = 5] = "Right";
    BGRATypeWriterAlignment[BGRATypeWriterAlignment["BottomLeft"] = 6] = "BottomLeft";
    BGRATypeWriterAlignment[BGRATypeWriterAlignment["Bottom"] = 7] = "Bottom";
    BGRATypeWriterAlignment[BGRATypeWriterAlignment["BottomRight"] = 8] = "BottomRight";
})(BGRATypeWriterAlignment || (BGRATypeWriterAlignment = {}));
;
var BGRATypeWriterOutlineMode;
(function (BGRATypeWriterOutlineMode) {
    BGRATypeWriterOutlineMode[BGRATypeWriterOutlineMode["Path"] = 0] = "Path";
    BGRATypeWriterOutlineMode[BGRATypeWriterOutlineMode["Fill"] = 1] = "Fill";
    BGRATypeWriterOutlineMode[BGRATypeWriterOutlineMode["Stroke"] = 2] = "Stroke";
    BGRATypeWriterOutlineMode[BGRATypeWriterOutlineMode["FillOverStroke"] = 3] = "FillOverStroke";
    BGRATypeWriterOutlineMode[BGRATypeWriterOutlineMode["StrokeOverFill"] = 4] = "StrokeOverFill";
    BGRATypeWriterOutlineMode[BGRATypeWriterOutlineMode["FillThenStroke"] = 5] = "FillThenStroke";
    BGRATypeWriterOutlineMode[BGRATypeWriterOutlineMode["StrokeThenFill"] = 6] = "StrokeThenFill";
})(BGRATypeWriterOutlineMode || (BGRATypeWriterOutlineMode = {}));
;
var BGRACustomFontRenderer = (function () {
    function BGRACustomFontRenderer() {
    }
    BGRACustomFontRenderer.prototype.GetFontPixelMetric = function () {
        return new FontPixelMetric();
    };
    BGRACustomFontRenderer.prototype.TextSize = function (sUTF8) {
        return new Size();
    };
    BGRACustomFontRenderer.prototype.TextOut = function (ADest, x, y, sUTF8, c, align) {
    };
    BGRACustomFontRenderer.prototype.TextRect = function (ADest, ARect, x, y, sUTF8, style, c) {
    };
    return BGRACustomFontRenderer;
}());
var BGRATextOutImproveReadabilityMode;
(function (BGRATextOutImproveReadabilityMode) {
    BGRATextOutImproveReadabilityMode[BGRATextOutImproveReadabilityMode["Mask"] = 0] = "Mask";
    BGRATextOutImproveReadabilityMode[BGRATextOutImproveReadabilityMode["Normal"] = 1] = "Normal";
    BGRATextOutImproveReadabilityMode[BGRATextOutImproveReadabilityMode["ClearTypeRGB"] = 2] = "ClearTypeRGB";
    BGRATextOutImproveReadabilityMode[BGRATextOutImproveReadabilityMode["ClearTypeBGR"] = 3] = "ClearTypeBGR";
})(BGRATextOutImproveReadabilityMode || (BGRATextOutImproveReadabilityMode = {}));
;
function CleanTextOutString(s) {
    var result;
    return result;
}
function RemoveLineEnding(s, indexByte) {
    return false;
}
function RemoveLineEndingUTF8(s, indexUTF8) {
    var indexByte;
    return RemoveLineEnding(s, indexByte);
}
function BGRADefaultWordBreakHandler(ABefore, AAfter) {
}
var ResampleMode;
(function (ResampleMode) {
    ResampleMode[ResampleMode["SimpleStretch"] = 0] = "SimpleStretch";
    ResampleMode[ResampleMode["FineResample"] = 1] = "FineResample";
})(ResampleMode || (ResampleMode = {}));
;
var ResampleFilter;
(function (ResampleFilter) {
    ResampleFilter[ResampleFilter["Box"] = 0] = "Box";
    ResampleFilter[ResampleFilter["Linear"] = 1] = "Linear";
    ResampleFilter[ResampleFilter["HalfCosine"] = 2] = "HalfCosine";
    ResampleFilter[ResampleFilter["Cosine"] = 3] = "Cosine";
    ResampleFilter[ResampleFilter["Bicubic"] = 4] = "Bicubic";
    ResampleFilter[ResampleFilter["Mitchel"] = 5] = "Mitchel";
    ResampleFilter[ResampleFilter["Spline"] = 6] = "Spline";
    ResampleFilter[ResampleFilter["Lanczos2"] = 7] = "Lanczos2";
    ResampleFilter[ResampleFilter["Lanczos3"] = 8] = "Lanczos3";
    ResampleFilter[ResampleFilter["Lanczos4"] = 9] = "Lanczos4";
    ResampleFilter[ResampleFilter["BestQuality"] = 10] = "BestQuality";
})(ResampleFilter || (ResampleFilter = {}));
;
var BGRAPixel = (function () {
    function BGRAPixel(red, green, blue, alpha) {
        this.raw = new Uint8ClampedArray(4);
        this.red = red;
        this.green = green;
        this.blue = blue;
        this.alpha = alpha;
    }
    Object.defineProperty(BGRAPixel.prototype, "red", {
        get: function () {
            return this.raw[0];
        },
        set: function (v) {
            this.raw[0] = v;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(BGRAPixel.prototype, "green", {
        get: function () {
            return this.raw[1];
        },
        set: function (v) {
            this.raw[1] = v;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(BGRAPixel.prototype, "blue", {
        get: function () {
            return this.raw[2];
        },
        set: function (v) {
            this.raw[2] = v;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(BGRAPixel.prototype, "alpha", {
        get: function () {
            return this.raw[3];
        },
        set: function (v) {
            this.raw[3] = v;
        },
        enumerable: true,
        configurable: true
    });
    BGRAPixel.prototype.toString = function () {
        return "BGRA(" + this.red + "," + this.green + "," + this.blue + "," + this.alpha + ")";
    };
    BGRAPixel.prototype.Equals = function (other) {
        return other === this || (other.alpha == 0 && this.alpha == 0) ||
            (other.red == this.red && other.green == this.green && other.green == this.green && other.blue == this.blue);
    };
    return BGRAPixel;
}());
var ExpandedPixel = (function () {
    function ExpandedPixel(red, green, blue, alpha) {
        this.raw = new Uint16Array(4);
        this.red = red;
        this.green = green;
        this.blue = blue;
        this.alpha = alpha;
    }
    Object.defineProperty(ExpandedPixel.prototype, "red", {
        get: function () {
            return this.raw[0];
        },
        set: function (v) {
            this.raw[0] = v;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(ExpandedPixel.prototype, "green", {
        get: function () {
            return this.raw[1];
        },
        set: function (v) {
            this.raw[1] = v;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(ExpandedPixel.prototype, "blue", {
        get: function () {
            return this.raw[2];
        },
        set: function (v) {
            this.raw[2] = v;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(ExpandedPixel.prototype, "alpha", {
        get: function () {
            return this.raw[3];
        },
        set: function (v) {
            this.raw[3] = v;
        },
        enumerable: true,
        configurable: true
    });
    ExpandedPixel.prototype.toString = function () {
        return "BGRA(" + this.red + "," + this.green + "," + this.blue + "," + this.alpha + ")";
    };
    ExpandedPixel.prototype.Equals = function (other) {
        return other === this || (other.alpha == 0 && this.alpha == 0) ||
            (other.red == this.red && other.green == this.green && other.green == this.green && other.blue == this.blue);
    };
    return ExpandedPixel;
}());
function BGRA(red, green, blue, alpha) {
    return new BGRAPixel(red, green, blue, alpha == undefined ? 255 : alpha);
}
function GetIntensity(c) {
    var result;
    result = c.red;
    if (c.green > result) {
        result = c.green;
    }
    if (c.blue > result) {
        result = c.blue;
    }
    if (c.constructor.name == 'BGRAPixel') {
        return GammaExpansionTab[result];
    }
}
function SetIntensity(c, intensity) {
    var curIntensity;
    var result;
    var compress;
    if (c.constructor.name == 'BGRAPixel') {
        result = new BGRAPixel(0, 0, 0, 0);
        c = GammaExpansion(c);
        compress = true;
    }
    else {
        result = new ExpandedPixel(0, 0, 0, 0);
    }
    curIntensity = GetIntensity(c);
    if (curIntensity = 0) {
        result.red = intensity;
        result.green = intensity;
        result.blue = intensity;
        result.alpha = c.alpha;
    }
    else {
        result.red = Math.round((c.red * intensity + (curIntensity >> 1)) / curIntensity);
        result.green = Math.round((c.green * intensity + (curIntensity >> 1)) / curIntensity);
        result.blue = Math.round((c.blue * intensity + (curIntensity >> 1)) / curIntensity);
        result.alpha = c.alpha;
    }
    if (compress) {
        return GammaCompression(c);
    }
    return result;
}
var BGRAPixelTransparent = new BGRAPixel(0, 0, 0, 0);
var BGRAWhite = new BGRAPixel(255, 255, 255, 255);
var BGRABlack = new BGRAPixel(0, 0, 0, 255);
var GammaExpansionTab = new Uint16Array(256);
var GammaCompressionTab = new Uint8ClampedArray(65536);
var DrawMode;
(function (DrawMode) {
    DrawMode[DrawMode["Set"] = 0] = "Set";
    DrawMode[DrawMode["SetExceptTransparent"] = 1] = "SetExceptTransparent";
    DrawMode[DrawMode["LinearBlend"] = 2] = "LinearBlend";
    DrawMode[DrawMode["DrawWithTransparency"] = 3] = "DrawWithTransparency";
    DrawMode[DrawMode["Xor"] = 4] = "Xor";
})(DrawMode || (DrawMode = {}));
;
var GammaExpFactor;
var redWeightShl10 = 306;
var greenWeightShl10 = 601;
var blueWeightShl10 = 117;
function BGRASetGamma(AGamma) {
    var GammaLinearFactor;
    var i;
    var j;
    var prevpos;
    var nextpos;
    var midpos;
    GammaExpFactor = AGamma;
    GammaLinearFactor = 65535 / Math.pow(255, GammaExpFactor);
    GammaExpansionTab[0] = 0;
    GammaCompressionTab[0] = 0;
    nextpos = 0;
    for (i = 0; i <= 255; i++) {
        prevpos = nextpos;
        midpos = Math.round(Math.pow(i, GammaExpFactor) * GammaLinearFactor);
        if (i == 255) {
            nextpos = 65536;
        }
        else {
            nextpos = Math.round(Math.pow(i + 0.5, GammaExpFactor) * GammaLinearFactor);
        }
        GammaExpansionTab[i] = midpos;
        for (j = prevpos; j <= nextpos - 1; j++) {
            GammaCompressionTab[j] = i;
        }
    }
}
function BGRAGetGamma() {
    return GammaExpFactor;
}
function GammaExpansion(c) {
    var result;
    result = new ExpandedPixel(GammaExpansionTab[c.red], GammaExpansionTab[c.green], GammaExpansionTab[c.blue], c.alpha << 8 + c.alpha);
    return result;
}
function GammaCompression(ec) {
    var result;
    result = new BGRAPixel(GammaCompressionTab[ec.red], GammaCompressionTab[ec.green], GammaCompressionTab[ec.blue], ec.alpha >> 8);
    return result;
}
function BGRAToGrayscale(c) {
    var ec;
    var gray;
    var cgray;
    if (c.alpha == 0) {
        return BGRAPixelTransparent;
    }
    ec = GammaExpansion(c);
    gray = (ec.red * redWeightShl10 + ec.green * greenWeightShl10 +
        ec.blue * blueWeightShl10 + 512) >> 10;
    cgray = GammaCompressionTab[gray];
    return new BGRAPixel(cgray, cgray, cgray, c.alpha);
}
BGRASetGamma(1.7);
var BGRABitmap = (function () {
    function BGRABitmap(canvas, width, height) {
        this.raw = canvas.getContext('2d').createImageData(width == undefined ? canvas.width : width, height == undefined ? canvas.height : height);
        this.rawModified = false;
        this.canvas = null;
        this.canvasModified = false;
        this.clipRect = Rect(0, 0, this.Width, this.Height);
        this.rowStride = this.Width * 4;
    }
    Object.defineProperty(BGRABitmap.prototype, "Width", {
        get: function () {
            return this.raw.width;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(BGRABitmap.prototype, "Height", {
        get: function () {
            return this.raw.height;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(BGRABitmap.prototype, "Data", {
        get: function () {
            if (this.canvasModified && (this.canvas != null)) {
                this.raw = this.canvas.getContext('2d').getImageData(0, 0, this.Width, this.Height);
                this.canvasModified = false;
            }
            return this.raw;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(BGRABitmap.prototype, "ClipRect", {
        get: function () {
            return this.clipRect;
        },
        set: function (v) {
            var newClip = v.Clone();
            newClip.Intersect(Rect(0, 0, this.Width, this.Height));
            if (!newClip.Equals(this.clipRect))
                this.clipRect = newClip;
        },
        enumerable: true,
        configurable: true
    });
    BGRABitmap.prototype.NoClip = function () {
        this.ClipRect = Rect(0, 0, this.Width, this.Height);
    };
    BGRABitmap.prototype.GetDataOffset = function (x, y) {
        return Math.floor(y) * this.rowStride + Math.floor(x) * 4;
    };
    Object.defineProperty(BGRABitmap.prototype, "CanvasElement", {
        get: function () {
            if (this.canvas == null) {
                this.canvas = document.createElement("canvas");
                this.canvas.width = this.Width;
                this.canvas.height = this.Height;
            }
            if (this.rawModified) {
                var ctx = this.canvas.getContext('2d');
                ctx.clearRect(0, 0, this.Width, this.Height);
                ctx.putImageData(this.raw, 0, 0);
                this.rawModified = false;
            }
            return this.canvas;
        },
        enumerable: true,
        configurable: true
    });
    Object.defineProperty(BGRABitmap.prototype, "Canvas2D", {
        get: function () {
            this.canvasModified = true;
            return this.CanvasElement.getContext('2d');
        },
        enumerable: true,
        configurable: true
    });
    BGRABitmap.prototype.Fill = function (color) {
        var data = this.Data;
        for (var i = 0; i < data.data.length; i += 4) {
            data.data[i] = color.red;
            data.data[i + 1] = color.green;
            data.data[i + 2] = color.blue;
            data.data[i + 3] = color.alpha;
        }
        this.InvalidateBitmap();
    };
    BGRABitmap.prototype.CheckClippedRectBounds = function (p1, p2) {
        var temp;
        if (p1.x > p2.x) {
            temp = p1.x;
            p1.x = p2.x;
            p2.x = temp;
        }
        if (p1.y > p2.y) {
            temp = p1.y;
            p1.y = p2.y;
            p2.y = temp;
        }
        if ((p1.x >= this.clipRect.Right) || (p2.x <= this.clipRect.Left) || (p1.y >= this.clipRect.Bottom) || (p2.y <= this.clipRect.Top)) {
            return false;
        }
        if (p1.x < this.clipRect.Left) {
            p1.x = this.clipRect.Left;
        }
        if (p2.x > this.clipRect.Right) {
            p2.x = this.clipRect.Right;
        }
        if (p1.y < this.clipRect.Top) {
            p1.y = this.clipRect.Top;
        }
        if (p2.y > this.clipRect.Bottom) {
            p2.y = this.clipRect.Bottom;
        }
        if ((p2.x - p1.x <= 0) || (p2.y - p1.y <= 0)) {
            return false;
        }
        else {
            return true;
        }
    };
    BGRABitmap.prototype.FillRect = function (x, y, x2, y2, c, mode) {
        var yb;
        var tx;
        var offset;
        var p1;
        var p2;
        p1 = new Point(x, y);
        p2 = new Point(x2, y2);
        if (!this.CheckClippedRectBounds(p1, p2)) {
            return;
        }
        x = p1.x;
        y = p1.y;
        x2 = p2.x;
        y2 = p2.y;
        tx = x2 - x;
        x2--;
        y2--;
        if (mode == DrawMode.SetExceptTransparent) {
            if (c.alpha == 255) {
            }
        }
        else {
            if ((mode != DrawMode.Set) && (mode != DrawMode.Xor) && (c.alpha = 0)) {
                return;
            }
            switch (mode) {
                case DrawMode.LinearBlend: {
                    break;
                }
                case DrawMode.DrawWithTransparency: {
                    break;
                }
                case DrawMode.Set: {
                    break;
                }
                case DrawMode.Xor: {
                    break;
                }
            }
            this.InvalidateBitmap;
        }
    };
    BGRABitmap.prototype.SetPixel = function (x, y, color) {
        if (!this.clipRect.Contains(x, y))
            return;
        var data = this.Data;
        var offset = this.GetDataOffset(x, y);
        data.data[offset] = color.red;
        data.data[offset + 1] = color.green;
        data.data[offset + 2] = color.blue;
        data.data[offset + 3] = color.alpha;
        this.InvalidateBitmap();
    };
    BGRABitmap.prototype.SetHorizLine = function (x, y, x2, color) {
        if (y < this.clipRect.Top || y >= this.clipRect.Bottom + 1)
            return;
        if (x2 < x) {
            var temp = x;
            x = x2;
            x2 = temp;
        }
        if (x >= this.clipRect.Right || x2 < this.clipRect.Left)
            return;
        if (x < this.clipRect.Left)
            x = this.clipRect.Left;
        if (x2 > this.clipRect.Right)
            x2 = this.clipRect.Right;
        var data = this.Data;
        var offset = this.GetDataOffset(x, y);
        var red = color.red, green = color.green, blue = color.blue, alpha = color.alpha;
        while (x <= x2) {
            data.data[offset] = red;
            data.data[offset + 1] = green;
            data.data[offset + 2] = blue;
            data.data[offset + 3] = alpha;
            offset += 4;
            x += 1;
        }
        this.InvalidateBitmap();
    };
    BGRABitmap.prototype.SetVertLine = function (x, y, y2, color) {
        if (x < this.clipRect.Left || x >= this.clipRect.Right + 1)
            return;
        if (y2 < y) {
            var temp = y;
            y = y2;
            y2 = temp;
        }
        if (y >= this.clipRect.Bottom || y2 < this.clipRect.Top)
            return;
        if (y < this.clipRect.Top)
            y = this.clipRect.Top;
        if (y2 > this.clipRect.Bottom)
            y2 = this.clipRect.Bottom;
        var data = this.Data;
        var offset = this.GetDataOffset(x, y);
        var red = color.red, green = color.green, blue = color.blue, alpha = color.alpha;
        while (y <= y2) {
            data.data[offset] = red;
            data.data[offset + 1] = green;
            data.data[offset + 2] = blue;
            data.data[offset + 3] = alpha;
            offset += this.rowStride;
            y += 1;
        }
        this.InvalidateBitmap();
    };
    BGRABitmap.prototype.SetRectangle = function (x, y, x2, y2, color) {
        if (y == y2) {
            this.SetHorizLine(x, y, x2, color);
            return;
        }
        if (x == x2) {
            this.SetVertLine(x, y, y2, color);
            return;
        }
        if (y > y2) {
            var temp = y;
            y = y2;
            y2 = temp;
        }
        this.SetHorizLine(x, y, x2, color);
        this.SetHorizLine(x, y2, x2, color);
        this.SetVertLine(x, y + 1, y2 - 1, color);
        this.SetVertLine(x2, y + 1, y2 - 1, color);
    };
    BGRABitmap.prototype.SetFilterGrayscale = function () {
        var data = this.Data;
        for (var i = 0; i < data.data.length; i += 4) {
            var luma = BGRAToGrayscale(BGRA(data.data[i], data.data[i + 1], data.data[i + 2], data.data[i + 3]));
            data.data[i] = luma.red;
            data.data[i + 1] = luma.green;
            data.data[i + 2] = luma.blue;
            data.data[i + 3] = luma.alpha;
        }
        this.InvalidateBitmap();
    };
    BGRABitmap.prototype.SetFilterInvert = function () {
        var data = this.Data;
        for (var i = 0; i < data.data.length; i += 4) {
            data.data[i] = 255 - data.data[i];
            data.data[i + 1] = 255 - data.data[i + 1];
            data.data[i + 2] = 255 - data.data[i + 2];
        }
        this.InvalidateBitmap();
    };
    BGRABitmap.prototype.SetFilterGameBoy = function () {
        var data = this.Data;
        for (var i = 0; i < data.data.length; i += 4) {
            var c = data.data[i] + data.data[i + 1] + data.data[i + 2];
            if (c <= 382) {
                if (c <= 191) {
                    data.data[i] = 0;
                    data.data[i + 1] = 80;
                    data.data[i + 2] = 32;
                }
                else {
                    data.data[i] = 0;
                    data.data[i + 1] = 104;
                    data.data[i + 2] = 24;
                }
            }
            else {
                if (c <= 573) {
                    data.data[i] = 0;
                    data.data[i + 1] = 176;
                    data.data[i + 2] = 0;
                }
                else {
                    data.data[i] = 112;
                    data.data[i + 1] = 224;
                    data.data[i + 2] = 48;
                }
            }
        }
        this.InvalidateBitmap();
    };
    BGRABitmap.prototype.SetFilterBrightness = function (adjustment) {
        var data = this.Data;
        for (var i = 0; i < data.data.length; i += 4) {
            data.data[i] += adjustment;
            data.data[i + 1] += adjustment;
            data.data[i + 2] += adjustment;
        }
        this.InvalidateBitmap();
    };
    BGRABitmap.prototype.SetFilterThreshold = function (threshold) {
        var data = this.Data;
        for (var i = 0; i < data.data.length; i += 4) {
            var luma = 0.299 * data.data[i] + 0.587 * data.data[i + 1] + 0.114 * data.data[i + 2];
            luma = (Math.round(luma) >= threshold) ? 255 : 0;
            data.data[i] += luma;
            data.data[i + 1] += luma;
            data.data[i + 2] += luma;
        }
        this.InvalidateBitmap();
    };
    BGRABitmap.prototype.InvalidateBitmap = function () {
        this.rawModified = true;
    };
    BGRABitmap.prototype.Draw = function (canvas, x, y) {
        canvas.getContext('2d').putImageData(this.Data, x, y);
    };
    return BGRABitmap;
}());
