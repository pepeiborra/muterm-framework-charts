
{- |
   Inlined here from graphviz-2999.1.0.2

   Module      : Data.GraphViz.Attributes
   Description : Definition of the GraphViz attributes.
   Copyright   : (c) Matthew Sackman, Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module defines the various attributes that different parts of
   a GraphViz graph can have.  These attributes are based on the
   documentation found at:

     <http://graphviz.org/doc/info/attrs.html>

   For more information on usage, etc. please see that document.

   A summary of known current constraints\/limitations\/differences:

   * Parsing of quoted strings might not always work if they are a
     sub-part of another Attribute (e.g. a quoted name in 'LayerList').
     In fact, parsing with quotes is iffy for everything; specifically
     when they are and aren't allowed.

   * 'ColorScheme' is ignored when parsing 'Color' values

   * ColorList and PointfList are defined as actual lists (but
     'LayerList' is not).

   * A lot of values have a possible value of @"none"@.  These now
     have custom constructors.  In fact, most constructors have been
     expanded upon to give an idea of what they represent rather than
     using generic terms.

   * @PointF@ and 'Point' have been combined, and feature support for pure
     'Int'-based co-ordinates as well as 'Double' ones (i.e. no floating
     point-only points for Point).  The optional '!' and third value
     for Point are not available.

   * 'Rect' uses two 'Point' values to denote the lower-left and
     top-right corners.

   * The two 'LabelLoc' attributes have been combined.

   * The defined 'LayerSep' is not used to parse 'LayerRange' or
     'LayerList'; the default (@[' ', ':', '\t']@) is instead used.

   * @SplineType@ has been replaced with @['Spline']@.

   * Only polygon-based 'Shape's are available.

   * Device-dependent 'StyleName' values are not available.

   * 'PortPos' only has the 'CompassPoint' option, not
     @PortName[:CompassPoint]@ (since record shapes aren't allowed,
     and parsing HTML-like labels could be problematic).

   * Not every 'Attribute' is fully documented/described.  In
     particular, a lot of them are listed as having a 'String' value,
     when actually only certain Strings are allowed.

   * Deprecated 'Overlap' algorithms are not defined.

 -}

module Data.GraphViz.Attributes where

import Data.Char(isDigit, isHexDigit)
import Data.Word
import Numeric
import Control.Monad
import Data.Maybe

-- -----------------------------------------------------------------------------

{- |

   These attributes have been implemented in a /permissive/ manner:
   that is, rather than split them up based on which type of value
   they are allowed, they have all been included in the one data type,
   with functions to determine if they are indeed valid for what
   they're being applied to.

   To interpret the /Valid for/ listings:

     [@G@] Valid for Graphs.

     [@C@] Valid for Clusters.

     [@S@] Valid for Sub-Graphs (and also Clusters).

     [@N@] Valid for Nodes.

     [@E@] Valid for Edges.

   Note also that the default values are taken from the specification
   page listed above, and might not correspond fully with the names of
   the permitted values.
-}
data Attribute
    = Damping Double                   -- ^ /Valid for/: G; /Default/: 0.99; /Minimum/: 0.0; /Notes/: neato only
    | K Double                         -- ^ /Valid for/: GC; /Default/: 0.3; /Minimum/: 0; /Notes/: sfdp, fdp only
    | URL URL                          -- ^ /Valid for/: ENGC; /Default/: \<none\>; /Notes/: svg, postscript, map only
    | ArrowHead ArrowType              -- ^ /Valid for/: E; /Default/: Normal
    | ArrowSize Double                 -- ^ /Valid for/: E; /Default/: 1.0; /Minimum/: 0.0
    | ArrowTail ArrowType              -- ^ /Valid for/: E; /Default/: Normal
    | Aspect AspectType                -- ^ /Valid for/: G; /Notes/: dot only
    | Bb Rect                          -- ^ /Valid for/: G; /Notes/: write only
    | BgColor Color                    -- ^ /Valid for/: GC; /Default/: \<none\>
    | Center Bool                      -- ^ /Valid for/: G; /Default/: false
    | Charset String                   -- ^ /Valid for/: G; /Default/: \"UTF-8\"
    | ClusterRank ClusterMode          -- ^ /Valid for/: G; /Default/: local; /Notes/: dot only
    | Color [Color]                    -- ^ /Valid for/: ENC; /Default/: black
    | ColorScheme String               -- ^ /Valid for/: ENCG; /Default/: \"\"
    | Comment String                   -- ^ /Valid for/: ENG; /Default/: \"\"
    | Compound Bool                    -- ^ /Valid for/: G; /Default/: false; /Notes/: dot only
    | Concentrate Bool                 -- ^ /Valid for/: G; /Default/: false
    | Constraint Bool                  -- ^ /Valid for/: E; /Default/: true; /Notes/: dot only
    | Decorate Bool                    -- ^ /Valid for/: E; /Default/: false
    | DefaultDist Double               -- ^ /Valid for/: G; /Default/: 1+(avg. len)*sqrt(|V|); /Minimum/: epsilon; /Notes/: neato only
    | Dim Int                          -- ^ /Valid for/: G; /Default/: 2; /Minimum/: 2; /Notes/: sfdp, fdp, neato only
    | Dimen Int                        -- ^ /Valid for/: G; /Default/: 2; /Minimum/: 2; /Notes/: sfdp, fdp, neato only
    | Dir DirType                      -- ^ /Valid for/: E; /Default/: forward(directed)/none(undirected)
    | DirEdgeConstraints DEConstraints -- ^ /Valid for/: G; /Default/: false; /Notes/: neato only
    | Distortion Double                -- ^ /Valid for/: N; /Default/: 0.0; /Minimum/: -100.0
    | DPI Double                       -- ^ /Valid for/: G; /Default/: 96.0 | 0.0; /Notes/: svg, bitmap output only
    | EdgeURL URL                      -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, map only
    | EdgeTarget String                -- ^ /Valid for/: E; /Default/: \<none\>; /Notes/: svg, map only
    | EdgeTooltip String               -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, cmap only
    | Epsilon Double                   -- ^ /Valid for/: G; /Default/: .0001 * # nodes(mode == KK) | .0001(mode == major); /Notes/: neato only
    | ESep DPoint                      -- ^ /Valid for/: G; /Default/: +3; /Notes/: not dot
    | FillColor Color                  -- ^ /Valid for/: NC; /Default/: lightgrey(nodes) | black(clusters)
    | FixedSize Bool                   -- ^ /Valid for/: N; /Default/: false
    | FontColor Color                  -- ^ /Valid for/: ENGC; /Default/: black
    | FontName String                  -- ^ /Valid for/: ENGC; /Default/: \"Times-Roman\"
    | FontNames String                 -- ^ /Valid for/: G; /Default/: \"\"; /Notes/: svg only
    | FontPath String                  -- ^ /Valid for/: G; /Default/: system-dependent
    | FontSize Double                  -- ^ /Valid for/: ENGC; /Default/: 14.0; /Minimum/: 1.0
    | Group String                     -- ^ /Valid for/: N; /Default/: \"\"; /Notes/: dot only
    | HeadURL URL                      -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, map only
    | HeadClip Bool                    -- ^ /Valid for/: E; /Default/: true
    | HeadLabel Label                  -- ^ /Valid for/: E; /Default/: \"\"
    | HeadPort PortPos                 -- ^ /Valid for/: E; /Default/: center
    | HeadTarget QuotedString          -- ^ /Valid for/: E; /Default/: \<none\>; /Notes/: svg, map only
    | HeadTooltip QuotedString         -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, cmap only
    | Height Double                    -- ^ /Valid for/: N; /Default/: 0.5; /Minimum/: 0.02
    | ID Label                         -- ^ /Valid for/: GNE; /Default/: \"\"; /Notes/: svg, postscript, map only
    | Image String                     -- ^ /Valid for/: N; /Default/: \"\"
    | ImageScale ScaleType             -- ^ /Valid for/: N; /Default/: false
    | Label Label                      -- ^ /Valid for/: ENGC; /Default/: \"\N\" (nodes) Nothing | \"\" (otherwise) Nothing
    | LabelURL URL                     -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, map only
    | LabelAngle Double                -- ^ /Valid for/: E; /Default/: -25.0; /Minimum/: -180.0
    | LabelDistance Double             -- ^ /Valid for/: E; /Default/: 1.0; /Minimum/: 0.0
    | LabelFloat Bool                  -- ^ /Valid for/: E; /Default/: false
    | LabelFontColor Color             -- ^ /Valid for/: E; /Default/: black
    | LabelFontName String             -- ^ /Valid for/: E; /Default/: \"Times-Roman\"
    | LabelFontSize Double             -- ^ /Valid for/: E; /Default/: 14.0; /Minimum/: 1.0
    | LabelJust Justification          -- ^ /Valid for/: GC; /Default/: \"c\"
    | LabelLoc VerticalPlacement       -- ^ /Valid for/: GCN; /Default/: \"t\"(clusters) | \"b\"(root graphs) | \"c\"(clusters)
    | LabelTarget String               -- ^ /Valid for/: E; /Default/: \<none\>; /Notes/: svg, map only
    | LabelTooltip String              -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, cmap only
    | Landscape Bool                   -- ^ /Valid for/: G; /Default/: false
    | Layer LayerRange                 -- ^ /Valid for/: EN; /Default/: \"\"
    | Layers LayerList                 -- ^ /Valid for/: G; /Default/: \"\"
    | LayerSep String                  -- ^ /Valid for/: G; /Default/: \" :\t\"
    | Layout String                    -- ^ /Valid for/: G; /Default/: \"\"
    | Len Double                       -- ^ /Valid for/: E; /Default/: 1.0(neato)/0.3(fdp); /Notes/: fdp, neato only
    | Levels Int                       -- ^ /Valid for/: G; /Default/: MAXINT; /Minimum/: 0.0; /Notes/: sfdp only
    | LevelsGap Double                 -- ^ /Valid for/: G; /Default/: 0.0; /Notes/: neato only
    | LHead String                     -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: dot only
    | LP Point                         -- ^ /Valid for/: EGC; /Notes/: write only
    | LTail String                     -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: dot only
    | Margin DPoint                    -- ^ /Valid for/: NG; /Default/: \<device-dependent\>
    | MaxIter Int                      -- ^ /Valid for/: G; /Default/: 100 * # nodes(mode == KK) | 200(mode == major) | 600(fdp); /Notes/: fdp, neato only
    | MCLimit Double                   -- ^ /Valid for/: G; /Default/: 1.0; /Notes/: dot only
    | MinDist Double                   -- ^ /Valid for/: G; /Default/: 1.0; /Minimum/: 0.0; /Notes/: circo only
    | MinLen Int                       -- ^ /Valid for/: E; /Default/: 1; /Minimum/: 0; /Notes/: dot only
    | Mode String                      -- ^ /Valid for/: G; /Default/: \"major\"; /Notes/: neato only
    | Model String                     -- ^ /Valid for/: G; /Default/: \"shortpath\"; /Notes/: neato only
    | Mosek Bool                       -- ^ /Valid for/: G; /Default/: false; /Notes/: neato only; requires the Mosek software
    | NodeSep Double                   -- ^ /Valid for/: G; /Default/: 0.25; /Minimum/: 0.02; /Notes/: dot only
    | NoJustify Bool                   -- ^ /Valid for/: GCNE; /Default/: false
    | Normalize Bool                   -- ^ /Valid for/: G; /Default/: false; /Notes/: not dot
    | Nslimit Double                   -- ^ /Valid for/: G; /Notes/: dot only
    | Nslimit1 Double                  -- ^ /Valid for/: G; /Notes/: dot only
    | Ordering String                  -- ^ /Valid for/: G; /Default/: \"\"; /Notes/: dot only
    | Orientation Double               -- ^ /Valid for/: N; /Default/: 0.0; /Minimum/: 360.0
    | OrientationGraph String          -- ^ /Valid for/: G; /Default/: \"\"; /Notes/: Landscape if \"[lL]*\" and rotate not defined
    | OutputOrder OutputMode           -- ^ /Valid for/: G; /Default/: breadthfirst
    | Overlap Overlap                  -- ^ /Valid for/: G; /Default/: true; /Notes/: not dot
    | OverlapScaling Double            -- ^ /Valid for/: G; /Default/: -4; /Minimum/: -1.0e10; /Notes/: prism only
    | Pack Pack                        -- ^ /Valid for/: G; /Default/: false; /Notes/: not dot
    | PackMode PackMode                -- ^ /Valid for/: G; /Default/: node; /Notes/: not dot
    | Pad DPoint                       -- ^ /Valid for/: G; /Default/: 0.0555 (4 points)
    | Page Point                       -- ^ /Valid for/: G
    | PageDir PageDir                  -- ^ /Valid for/: G; /Default/: BL
    | PenColor Color                   -- ^ /Valid for/: C; /Default/: black
    | PenWidth Double                  -- ^ /Valid for/: CNE; /Default/: 1.0; /Minimum/: 0.0
    | Peripheries Int                  -- ^ /Valid for/: NC; /Default/: shape default(nodes) | 1(clusters); /Minimum/: 0
    | Pin Bool                         -- ^ /Valid for/: N; /Default/: false; /Notes/: fdp, neato only
    | Pos Pos                          -- ^ /Valid for/: EN
    | QuadTree QuadType                -- ^ /Valid for/: G; /Default/: \"normal\"; /Notes/: sfdp only
    | Quantum Double                   -- ^ /Valid for/: G; /Default/: 0.0; /Minimum/: 0.0
    | Rank RankType                    -- ^ /Valid for/: S; /Notes/: dot only
    | RankDir RankDir                  -- ^ /Valid for/: G; /Default/: TB; /Notes/: dot only
    | Ranksep Double                   -- ^ /Valid for/: G; /Default/: 0.5(dot) | 1.0(twopi); /Minimum/: 0.02; /Notes/: twopi, dot only
    | Ratio Ratios                     -- ^ /Valid for/: G
    | Rects Rect                       -- ^ /Valid for/: N; /Notes/: write only
    | Regular Bool                     -- ^ /Valid for/: N; /Default/: false
    | ReMinCross Bool                  -- ^ /Valid for/: G; /Default/: false; /Notes/: dot only
    | RepulsiveForce Double            -- ^ /Valid for/: G; /Default/: 1.0; /Minimum/: 0.0; /Notes/: sfdp only
    | Resolution Double                -- ^ /Valid for/: G; /Default/: 96.0 | 0.0; /Notes/: svg, bitmap output only
    | Root Root                        -- ^ /Valid for/: GN; /Default/: \"\"(graphs) | false(nodes); /Notes/: circo, twopi only
    | Rotate Int                       -- ^ /Valid for/: G; /Default/: 0
    | SameHead String                  -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: dot only
    | SameTail String                  -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: dot only
    | SamplePoints Int                 -- ^ /Valid for/: N; /Default/: 8(output) | 20(overlap and image maps)
    | SearchSize Int                   -- ^ /Valid for/: G; /Default/: 30; /Notes/: dot only
    | Sep DPoint                       -- ^ /Valid for/: G; /Default/: +4; /Notes/: not dot
    | Shape Shape                      -- ^ /Valid for/: N; /Default/: ellipse
    | ShapeFile String                 -- ^ /Valid for/: N; /Default/: \"\"
    | ShowBoxes Int                    -- ^ /Valid for/: ENG; /Default/: 0; /Minimum/: 0; /Notes/: dot only
    | Sides Int                        -- ^ /Valid for/: N; /Default/: 4; /Minimum/: 0
    | Size Point                       -- ^ /Valid for/: G
    | Skew Double                      -- ^ /Valid for/: N; /Default/: 0.0; /Minimum/: -100.0
    | Smoothing SmoothType             -- ^ /Valid for/: G; /Default/: \"none\"; /Notes/: sfdp only
    | SortV Int                        -- ^ /Valid for/: GCN; /Default/: 0; /Minimum/: 0
    | Splines EdgeType                 -- ^ /Valid for/: G
    | Start StartType                  -- ^ /Valid for/: G; /Default/: \"\"; /Notes/: fdp, neato only
    | Style Style                      -- ^ /Valid for/: ENC
    | StyleSheet String                -- ^ /Valid for/: G; /Default/: \"\"; /Notes/: svg only
    | TailURL URL                      -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, map only
    | TailClip Bool                    -- ^ /Valid for/: E; /Default/: true
    | TailLabel Label                  -- ^ /Valid for/: E; /Default/: \"\"
    | TailPort PortPos                 -- ^ /Valid for/: E; /Default/: center
    | TailTarget String                -- ^ /Valid for/: E; /Default/: \<none\>; /Notes/: svg, map only
    | TailTooltip String               -- ^ /Valid for/: E; /Default/: \"\"; /Notes/: svg, cmap only
    | Target String                    -- ^ /Valid for/: ENGC; /Default/: \<none\>; /Notes/: svg, map only
    | Tooltip String                   -- ^ /Valid for/: NEC; /Default/: \"\"; /Notes/: svg, cmap only
    | TrueColor Bool                   -- ^ /Valid for/: G; /Notes/: bitmap output only
    | Vertices [Point]                 -- ^ /Valid for/: N; /Notes/: write only
    | ViewPort ViewPort                -- ^ /Valid for/: G; /Default/: \"\"
    | VoroMargin Double                -- ^ /Valid for/: G; /Default/: 0.05; /Minimum/: 0.0; /Notes/: not dot
    | Weight Double                    -- ^ /Valid for/: E; /Default/: 1.0; /Minimum/: 0(dot) | 1(neato,fdp,sfdp)
    | Width Double                     -- ^ /Valid for/: N; /Default/: 0.75; /Minimum/: 0.01
    | Z Double                         -- ^ /Valid for/: N; /Default/: 0.0; /Minimum/: -MAXFLOAT | -1000
      deriving (Eq, Read)

instance Show Attribute where
    show (Damping v)            = "Damping=" ++ show v
    show (K v)                  = "K=" ++ show v
    show (URL v)                = "URL=" ++ show v
    show (ArrowHead v)          = "arrowhead=" ++ show v
    show (ArrowSize v)          = "arrowsize=" ++ show v
    show (ArrowTail v)          = "arrowtail=" ++ show v
    show (Aspect v)             = "aspect=" ++ show v
    show (Bb v)                 = "bb=" ++ show v
    show (BgColor v)            = "bgcolor=" ++ show v
    show (Center v)             = "center=" ++ show v
    show (Charset v)            = "charset=" ++ v
    show (ClusterRank v)        = "clusterrank=" ++ show v
    show (Color v)              = "color=" ++ show v
    show (ColorScheme v)        = "colorscheme=" ++ v
    show (Comment v)            = "comment=" ++ v
    show (Compound v)           = "compound=" ++ show v
    show (Concentrate v)        = "concentrate=" ++ show v
    show (Constraint v)         = "constraint=" ++ show v
    show (Decorate v)           = "decorate=" ++ show v
    show (DefaultDist v)        = "defaultdist=" ++ show v
    show (Dim v)                = "dim=" ++ show v
    show (Dimen v)              = "dimen=" ++ show v
    show (Dir v)                = "dir=" ++ show v
    show (DirEdgeConstraints v) = "diredgeconstraints=" ++ show v
    show (Distortion v)         = "distortion=" ++ show v
    show (DPI v)                = "dpi=" ++ show v
    show (EdgeURL v)            = "edgeURL=" ++ show v
    show (EdgeTarget v)         = "edgetarget=" ++ v
    show (EdgeTooltip v)        = "edgetooltip=" ++ v
    show (Epsilon v)            = "epsilon=" ++ show v
    show (ESep v)               = "esep=" ++ show v
    show (FillColor v)          = "fillcolor=" ++ show v
    show (FixedSize v)          = "fixedsize=" ++ show v
    show (FontColor v)          = "fontcolor=" ++ show v
    show (FontName v)           = "fontname=" ++ v
    show (FontNames v)          = "fontnames=" ++ v
    show (FontPath v)           = "fontpath=" ++ v
    show (FontSize v)           = "fontsize=" ++ show v
    show (Group v)              = "group=" ++ v
    show (HeadURL v)            = "headURL=" ++ show v
    show (HeadClip v)           = "headclip=" ++ show v
    show (HeadLabel v)          = "headlabel=" ++ show v
    show (HeadPort v)           = "headport=" ++ show v
    show (HeadTarget v)         = "headtarget=" ++ show v
    show (HeadTooltip v)        = "headtooltip=" ++ show v
    show (Height v)             = "height=" ++ show v
    show (ID v)                 = "id=" ++ show v
    show (Image v)              = "image=" ++ v
    show (ImageScale v)         = "imagescale=" ++ show v
    show (Label v)              = "label=" ++ show v
    show (LabelURL v)           = "labelURL=" ++ show v
    show (LabelAngle v)         = "labelangle=" ++ show v
    show (LabelDistance v)      = "labeldistance=" ++ show v
    show (LabelFloat v)         = "labelfloat=" ++ show v
    show (LabelFontColor v)     = "labelfontcolor=" ++ show v
    show (LabelFontName v)      = "labelfontname=" ++ v
    show (LabelFontSize v)      = "labelfontsize=" ++ show v
    show (LabelJust v)          = "labeljust=" ++ show v
    show (LabelLoc v)           = "labelloc=" ++ show v
    show (LabelTarget v)        = "labeltarget=" ++ v
    show (LabelTooltip v)       = "labeltooltip=" ++ v
    show (Landscape v)          = "landscape=" ++ show v
    show (Layer v)              = "layer=" ++ show v
    show (Layers v)             = "layers=" ++ show v
    show (LayerSep v)           = "layersep=" ++ v
    show (Layout v)             = "layout=" ++ v
    show (Len v)                = "len=" ++ show v
    show (Levels v)             = "levels=" ++ show v
    show (LevelsGap v)          = "levelsgap=" ++ show v
    show (LHead v)              = "lhead=" ++ v
    show (LP v)                 = "lp=" ++ show v
    show (LTail v)              = "ltail=" ++ v
    show (Margin v)             = "margin=" ++ show v
    show (MaxIter v)            = "maxiter=" ++ show v
    show (MCLimit v)            = "mclimit=" ++ show v
    show (MinDist v)            = "mindist=" ++ show v
    show (MinLen v)             = "minlen=" ++ show v
    show (Mode v)               = "mode=" ++ v
    show (Model v)              = "model=" ++ v
    show (Mosek v)              = "mosek=" ++ show v
    show (NodeSep v)            = "nodesep=" ++ show v
    show (NoJustify v)          = "nojustify=" ++ show v
    show (Normalize v)          = "normalize=" ++ show v
    show (Nslimit v)            = "nslimit=" ++ show v
    show (Nslimit1 v)           = "nslimit1=" ++ show v
    show (Ordering v)           = "ordering=" ++ v
    show (Orientation v)        = "orientation=" ++ show v
    show (OrientationGraph v)   = "orientation=" ++ v
    show (OutputOrder v)        = "outputorder=" ++ show v
    show (Overlap v)            = "overlap=" ++ show v
    show (OverlapScaling v)     = "overlap_scaling=" ++ show v
    show (Pack v)               = "pack=" ++ show v
    show (PackMode v)           = "packmode=" ++ show v
    show (Pad v)                = "pad=" ++ show v
    show (Page v)               = "page=" ++ show v
    show (PageDir v)            = "pagedir=" ++ show v
    show (PenColor v)           = "pencolor=" ++ show v
    show (PenWidth v)           = "penwidth=" ++ show v
    show (Peripheries v)        = "peripheries=" ++ show v
    show (Pin v)                = "pin=" ++ show v
    show (Pos v)                = "pos=" ++ show v
    show (QuadTree v)           = "quadtree=" ++ show v
    show (Quantum v)            = "quantum=" ++ show v
    show (Rank v)               = "rank=" ++ show v
    show (RankDir v)            = "rankdir=" ++ show v
    show (Ranksep v)            = "ranksep=" ++ show v
    show (Ratio v)              = "ratio=" ++ show v
    show (Rects v)              = "rects=" ++ show v
    show (Regular v)            = "regular=" ++ show v
    show (ReMinCross v)         = "remincross=" ++ show v
    show (RepulsiveForce v)     = "repulsiveforce=" ++ show v
    show (Resolution v)         = "resolution=" ++ show v
    show (Root v)               = "root=" ++ show v
    show (Rotate v)             = "rotate=" ++ show v
    show (SameHead v)           = "samehead=" ++ v
    show (SameTail v)           = "sametail=" ++ v
    show (SamplePoints v)       = "samplepoints=" ++ show v
    show (SearchSize v)         = "searchsize=" ++ show v
    show (Sep v)                = "sep=" ++ show v
    show (Shape v)              = "shape=" ++ show v
    show (ShapeFile v)          = "shapefile=" ++ v
    show (ShowBoxes v)          = "showboxes=" ++ show v
    show (Sides v)              = "sides=" ++ show v
    show (Size v)               = "size=" ++ show v
    show (Skew v)               = "skew=" ++ show v
    show (Smoothing v)          = "smoothing=" ++ show v
    show (SortV v)              = "sortv=" ++ show v
    show (Splines v)            = "splines=" ++ show v
    show (Start v)              = "start=" ++ show v
    show (Style v)              = "style=" ++ show v
    show (StyleSheet v)         = "stylesheet=" ++ v
    show (TailURL v)            = "tailURL=" ++ show v
    show (TailClip v)           = "tailclip=" ++ show v
    show (TailLabel v)          = "taillabel=" ++ show v
    show (TailPort v)           = "tailport=" ++ show v
    show (TailTarget v)         = "tailtarget=" ++ v
    show (TailTooltip v)        = "tailtooltip=" ++ v
    show (Target v)             = "target=" ++ v
    show (Tooltip v)            = "tooltip=" ++ v
    show (TrueColor v)          = "truecolor=" ++ show v
    show (Vertices v)           = "vertices=" ++ show v
    show (ViewPort v)           = "viewport=" ++ show v
    show (VoroMargin v)         = "voro_margin=" ++ show v
    show (Weight v)             = "weight=" ++ show v
    show (Width v)              = "width=" ++ show v
    show (Z v)                  = "z=" ++ show v

-- -----------------------------------------------------------------------------

newtype URL = UStr { urlString :: String }
    deriving (Eq, Read)

instance Show URL where
    show u = '<' : urlString u ++ ">"

-- -----------------------------------------------------------------------------

data ArrowType = Normal   | Inv
               | DotArrow | InvDot
               | ODot     | InvODot
               | NoArrow  | Tee
               | Empty    | InvEmpty
               | Diamond  | ODiamond
               | EDiamond | Crow
               | Box      | OBox
               | Open     | HalfOpen
               | Vee
                 deriving (Eq, Read)

instance Show ArrowType where
    show Normal   = "normal"
    show Inv      = "inv"
    show DotArrow = "dot"
    show InvDot   = "invdot"
    show ODot     = "odot"
    show InvODot  = "invodot"
    show NoArrow  = "none"
    show Tee      = "tee"
    show Empty    = "empty"
    show InvEmpty = "invempty"
    show Diamond  = "diamond"
    show ODiamond = "odiamond"
    show EDiamond = "ediamond"
    show Crow     = "crow"
    show Box      = "box"
    show OBox     = "obox"
    show Open     = "open"
    show HalfOpen = "halfopen"
    show Vee      = "vee"
-- -----------------------------------------------------------------------------

data AspectType = RatioOnly Double
                | RatioPassCount Double Int
                  deriving (Eq, Read)

instance Show AspectType where
    show (RatioOnly r)        = show r
    show (RatioPassCount r p) = show $ show r ++ ',' : show p

data Rect = Rect Point Point
            deriving (Eq, Read)

instance Show Rect where
    show (Rect p1 p2) = show $ show p1 ++ ',' : show p2

-- -----------------------------------------------------------------------------

data Color = RGB { red   :: Word8
                 , green :: Word8
                 , blue  :: Word8
                 }
           | RGBA { red   :: Word8
                  , green :: Word8
                  , blue  :: Word8
                  , alpha :: Word8
                  }
           | HSV { hue        :: Int
                 , saturation :: Int
                 , value      :: Int
                 }
           | ColorName String
             deriving (Eq, Read)

instance Show Color where
    show = show . showColor

    showList cs s = show $ go cs
        where
          go []      = s
          go [c]     = showColor c ++ s
          go (c:cs') = showColor c ++ ':' : go cs'

showColor :: Color -> String
showColor (RGB r g b)      = '#' : foldr showWord8Pad "" [r,g,b]
showColor (RGBA r g b a)   = '#' : foldr showWord8Pad "" [r,g,b,a]
showColor (HSV h s v)      = show h ++ " " ++ show s ++ " " ++ show v
showColor (ColorName name) = name

showWord8Pad :: Word8 -> String -> String
showWord8Pad w s = padding ++ simple ++ s
    where
      simple = showHex w ""
      padding = replicate count '0'
      count = 2 - findCols 1 w
      findCols :: Int -> Word8 -> Int
      findCols c n
          | n < 16 = c
          | otherwise = findCols (c+1) (n `div` 16)


data ClusterMode = Local
                 | Global
                 | NoCluster
                   deriving (Eq, Read)

instance Show ClusterMode where
    show Local     = "local"
    show Global    = "global"
    show NoCluster = "none"

data DirType = Forward | Back | Both | NoDir
               deriving (Eq, Read)

instance Show DirType where
    show Forward = "forward"
    show Back    = "back"
    show Both    = "both"
    show NoDir   = "none"

-- | Only when @mode=ipsep@.
data DEConstraints = DEBool Bool
                   | Hier
                     deriving (Eq, Read)

instance Show DEConstraints where
    show (DEBool b) = show b
    show Hier       = "hier"

-- | Either a 'Double' or a 'Point'.
data DPoint = DVal Double
            | PVal Point
             deriving (Eq, Read)

instance Show DPoint where
    show (DVal d) = show d
    show (PVal p) = show p

data Label = StrLabel String
           | URLLabel URL
             deriving (Eq, Read)

instance Show Label where
    show (StrLabel s) = s
    show (URLLabel u) = show u

data Point = Point Int Int
           | PointD Double Double
             deriving (Eq, Read)

instance Show Point where
    show = show . showPoint

showPoint :: Point -> String
showPoint (Point  x y) = show x ++ ',' : show y
showPoint (PointD x y) = show x ++ ',' : show y


data Overlap = KeepOverlaps
             | RemoveOverlaps
             | ScaleOverlaps
             | ScaleXYOverlaps
             | PrismOverlap (Maybe Int) -- ^ Only when sfdp is available, @Int@ is non-negative
             | CompressOverlap
             | VpscOverlap
             | IpsepOverlap -- ^ Only when @mode="ipsep"@
               deriving (Eq, Read)

instance Show Overlap where
    show KeepOverlaps     = "true"
    show RemoveOverlaps   = "false"
    show ScaleOverlaps    = "scale"
    show ScaleXYOverlaps  = "scalexy"
    show (PrismOverlap i) = maybe id (flip (++) . show) i $ "prism"
    show CompressOverlap  = "compress"
    show VpscOverlap      = "vpsc"
    show IpsepOverlap     = "ipsep"

data LayerRange = LRID LayerID
                | LRS LayerID String LayerID
                  deriving (Eq, Read)

instance Show LayerRange where
    show (LRID lid)        = show lid
    show (LRS id1 sep id2) = show $ show id1 ++ sep ++ show id2

data LayerID = AllLayers
             | LRInt Int
             | LRName String
               deriving (Eq, Read)

instance Show LayerID where
    show AllLayers   = "all"
    show (LRInt n)   = show n
    show (LRName nm) = nm


-- | The list represent (Separator, Name)
data LayerList = LL String [(String, String)]
                 deriving (Eq, Read)

instance Show LayerList where
    show (LL l1 ols) = l1 ++ concatMap (uncurry (++)) ols


data OutputMode = BreadthFirst | NodesFirst | EdgesFirst
                  deriving (Eq, Read)

instance Show OutputMode where
    show BreadthFirst = "breadthfirst"
    show NodesFirst = "nodesfirst"
    show EdgesFirst = "edgesfirst"


data Pack = DoPack
          | DontPack
          | PackMargin Int -- ^ If non-negative, then packs; otherwise doesn't.
            deriving (Eq, Read)

instance Show Pack where
    show DoPack         = "true"
    show DontPack       = "false"
    show (PackMargin m) = show m

data PackMode = PackNode
              | PackClust
              | PackGraph
              | PackArray Bool Bool (Maybe Int) -- ^ Sort by cols, sort
                                                -- by user, number of
                                                -- rows/cols
                deriving (Eq, Read)

instance Show PackMode where
    show PackNode           = "node"
    show PackClust          = "clust"
    show PackGraph          = "graph"
    show (PackArray c u mi) = addNum . isU . isC . isUnder
                              $ "array"
        where
          addNum = maybe id (flip (++) . show) mi
          isUnder = if c || u
                    then flip (++) "_"
                    else id
          isC = if c
                then flip (++) "c"
                else id
          isU = if u
                then flip (++) "u"
                else id

data Pos = PointPos Point
         | SplinePos [Spline]
           deriving (Eq, Read)

instance Show Pos where
    show (PointPos p)   = show p
    show (SplinePos ss) = show ss

-- | Controls how (and if) edges are represented.
data EdgeType = SplineEdges
              | LineEdges
              | NoEdges
              | PolyLine
              | CompoundEdge -- ^ fdp only
                deriving (Eq, Read)

instance Show EdgeType where
    show SplineEdges  = "true"
    show LineEdges    = "false"
    show NoEdges      = "\"\""
    show PolyLine     = "polyline"
    show CompoundEdge = "compound"

-- | Upper-case first character is major order;
--   lower-case second character is minor order.
data PageDir = Bl | Br | Tl | Tr | Rb | Rt | Lb | Lt
               deriving (Eq, Read)

instance Show PageDir where
    show Bl = "BL"
    show Br = "BR"
    show Tl = "TL"
    show Tr = "TR"
    show Rb = "RB"
    show Rt = "RT"
    show Lb = "LB"
    show Lt = "LT"

-- | The number of points in the list must be equivalent to 1 mod 3;
--   note that this is not checked.
data Spline = Spline (Maybe Point) (Maybe Point) [Point]
              deriving (Eq, Read)

instance Show Spline where
    show = show . showSpline

    showList ss o = show $ go ss
        where
          go []      = o
          go [s]     = showSpline s ++ o
          go (s:ss') = showSpline s ++ ';' : go ss'

showSpline                   :: Spline -> String
showSpline (Spline ms me ps) = addS . addE
                               . unwords
                               $ map showPoint ps
    where
      addP t = maybe id (\p -> (++) $ t : ',' : show p)
      addS = addP 's' ms
      addE = addP 'e' me

data QuadType = NormalQT
              | FastQT
              | NoQT
                deriving (Eq, Read)

instance Show QuadType where
    show NormalQT = "normal"
    show FastQT   = "fast"
    show NoQT     = "none"

-- | Specify the root node either as a Node attribute or a Graph attribute.
data Root = IsCentral -- ^ For Nodes only
          | NotCentral -- ^ For Nodes only
          | NodeName String -- ^ For Graphs only
            deriving (Eq, Read)

instance Show Root where
    show IsCentral    = "true"
    show NotCentral   = "false"
    show (NodeName n) = n

-- -----------------------------------------------------------------------------

data RankType = SameRank
              | MinRank
              | SourceRank
              | MaxRank
              | SinkRank
                deriving (Eq, Read)

instance Show RankType where
    show SameRank   = "same"
    show MinRank    = "min"
    show SourceRank = "source"
    show MaxRank    = "max"
    show SinkRank   = "sink"

-- -----------------------------------------------------------------------------

data RankDir = FromTop
             | FromLeft
             | FromBottom
             | FromRight
               deriving (Eq, Read)

instance Show RankDir where
    show FromTop    = "TB"
    show FromLeft   = "LR"
    show FromBottom = "BT"
    show FromRight  = "RL"

-- -----------------------------------------------------------------------------

data Shape
    = BoxShape
    | Polygon
    | Ellipse
    | Circle
    | PointShape
    | Egg
    | Triangle
    | Plaintext
    | DiamondShape
    | Trapezium
    | Parallelogram
    | House
    | Pentagon
    | Hexagon
    | Septagon
    | Octagon
    | Doublecircle
    | Doubleoctagon
    | Tripleoctagon
    | Invtriangle
    | Invtrapezium
    | Invhouse
    | Mdiamond
    | Msquare
    | Mcircle
    | Rectangle
    | NoShape
    | Note
    | Tab
    | Folder
    | Box3d
    | Component
      deriving (Eq, Read)

instance Show Shape where
    show BoxShape      = "box"
    show Polygon       = "polygon"
    show Ellipse       = "ellipse"
    show Circle        = "circle"
    show PointShape    = "point"
    show Egg           = "egg"
    show Triangle      = "triangle"
    show Plaintext     = "plaintext"
    show DiamondShape  = "diamond"
    show Trapezium     = "trapezium"
    show Parallelogram = "parallelogram"
    show House         = "house"
    show Pentagon      = "pentagon"
    show Hexagon       = "hexagon"
    show Septagon      = "septagon"
    show Octagon       = "octagon"
    show Doublecircle  = "doublecircle"
    show Doubleoctagon = "doubleoctagon"
    show Tripleoctagon = "tripleoctagon"
    show Invtriangle   = "invtriangle"
    show Invtrapezium  = "invtrapezium"
    show Invhouse      = "invhouse"
    show Mdiamond      = "mdiamond"
    show Msquare       = "msquare"
    show Mcircle       = "mcircle"
    show Rectangle     = "rectangle"
    show NoShape       = "none"
    show Note          = "note"
    show Tab           = "tab"
    show Folder        = "folder"
    show Box3d         = "box3d"
    show Component     = "component"

-- -----------------------------------------------------------------------------

data SmoothType = NoSmooth
                | AvgDist
                | GraphDist
                | PowerDist
                | RNG
                | Spring
                | TriangleSmooth
                  deriving (Eq, Read)

instance Show SmoothType where
    show NoSmooth       = "none"
    show AvgDist        = "avg_dist"
    show GraphDist      = "graph_dist"
    show PowerDist      = "power_dist"
    show RNG            = "rng"
    show Spring         = "spring"
    show TriangleSmooth = "triangle"

-- -----------------------------------------------------------------------------

-- | It it assumed that at least one of these is @Just{}@.
data StartType = ST (Maybe STStyle) (Maybe Int) -- Use a Word?
                 deriving (Eq, Read)

instance Show StartType where
    show (ST ms mi) = maybe id ((++) . show) ms
                      $ maybe "" show mi

data STStyle = RegularStyle
             | Self
             | Random
               deriving (Eq, Read)

instance Show STStyle where
    show RegularStyle = "regular"
    show Self         = "self"
    show Random       = "random"

-- -----------------------------------------------------------------------------

data Style = Stl StyleName (Maybe String)
             deriving (Eq, Read)

instance Show Style where
    show (Stl nm marg) = maybe snm
                               (\arg -> show $ snm ++ '(' : arg ++ ")")
                               marg
        where
          snm = show nm


data StyleName = Dashed    -- ^ Nodes and Edges
               | Dotted    -- ^ Nodes and Edges
               | Solid     -- ^ Nodes and Edges
               | Bold      -- ^ Nodes and Edges
               | Invisible -- ^ Nodes and Edges
               | Filled    -- ^ Nodes and Clusters
               | Diagonals -- ^ Nodes only
               | Rounded   -- ^ Nodes and Clusters
                 deriving (Eq, Read)

instance Show StyleName where
    show Filled    = "filled"
    show Invisible = "invis"
    show Diagonals = "diagonals"
    show Rounded   = "rounded"
    show Dashed    = "dashed"
    show Dotted    = "dotted"
    show Solid     = "solid"
    show Bold      = "bold"

-- -----------------------------------------------------------------------------

newtype PortPos = PP CompassPoint
    deriving (Eq, Read)

instance Show PortPos where
    show (PP cp) = show cp

data CompassPoint = North
                  | NorthEast
                  | East
                  | SouthEast
                  | South
                  | SouthWest
                  | West
                  | NorthWest
                  | CenterPoint
                  | NoCP
                    deriving (Eq, Read)

instance Show CompassPoint where
    show North       = "n"
    show NorthEast   = "ne"
    show East        = "e"
    show SouthEast   = "se"
    show South       = "s"
    show SouthWest   = "sw"
    show West        = "w"
    show NorthWest   = "nw"
    show CenterPoint = "c"
    show NoCP        = "_"

-- -----------------------------------------------------------------------------

data ViewPort = VP { wVal  :: Double
                   , hVal  :: Double
                   , zVal  :: Double
                   , focus :: Maybe FocusType
                   }
                deriving (Eq, Read)

instance Show ViewPort where
    show vp = show
              . maybe id (flip (++) . show) (focus vp)
              $ show (wVal vp)
              ++ ',' : show (hVal vp)
              ++ ',' : show (zVal vp)

data FocusType = XY Point
               | NodeFocus String
                 deriving (Eq, Read)

instance Show FocusType where
    show (XY p)        = showPoint p
    show (NodeFocus nm) = nm

-- -----------------------------------------------------------------------------

-- | Note that 'VCenter' is only valid for Nodes.
data VerticalPlacement = VTop
                       | VCenter
                       | VBottom
                         deriving (Eq, Read)

instance Show VerticalPlacement where
    show VTop    = "t"
    show VCenter = "c"
    show VBottom = "b"

-- -----------------------------------------------------------------------------

data ScaleType = UniformScale
               | NoScale
               | FillWidth
               | FillHeight
               | FillBoth
                 deriving (Eq, Read)

instance Show ScaleType where
    show UniformScale = "true"
    show NoScale      = "false"
    show FillWidth    = "width"
    show FillHeight   = "height"
    show FillBoth     = "both"

-- -----------------------------------------------------------------------------

data Justification = JLeft
                   | JRight
                   | JCenter
                     deriving (Eq, Read)

instance Show Justification where
    show JLeft   = "l"
    show JRight  = "r"
    show JCenter = "c"

-- -----------------------------------------------------------------------------

data Ratios = AspectRatio Double
            | FillRatio
            | CompressRatio
            | ExpandRatio
            | AutoRatio
              deriving (Eq, Read)

instance Show Ratios where
    show (AspectRatio r) = show r
    show FillRatio       = "fill"
    show CompressRatio   = "compress"
    show ExpandRatio     = "expand"
    show AutoRatio       = "auto"

-- -----------------------------------------------------------------------------

-- | Represents 'String's that definitely have quotes around them.
newtype QuotedString = QS { qStr :: String }
    deriving (Eq, Read)

instance Show QuotedString where
    show = show . qStr

-- Utility Functions

-- | Fold over 'Bool's.
bool       :: a -> a -> Bool -> a
bool t f b = if b
             then t
             else f
