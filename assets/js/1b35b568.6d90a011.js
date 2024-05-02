"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[173],{3905:(e,t,n)=>{n.d(t,{Zo:()=>u,kt:()=>h});var a=n(67294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var s=a.createContext({}),p=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},u=function(e){var t=p(e.components);return a.createElement(s.Provider,{value:t},e.children)},c="mdxType",m={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},d=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,s=e.parentName,u=l(e,["components","mdxType","originalType","parentName"]),c=p(n),d=r,h=c["".concat(s,".").concat(d)]||c[d]||m[d]||o;return n?a.createElement(h,i(i({ref:t},u),{},{components:n})):a.createElement(h,i({ref:t},u))}));function h(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,i=new Array(o);i[0]=d;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l[c]="string"==typeof e?e:r,i[1]=l;for(var p=2;p<o;p++)i[p]=n[p];return a.createElement.apply(null,i)}return a.createElement.apply(null,n)}d.displayName="MDXCreateElement"},85760:(e,t,n)=>{n.r(t),n.d(t,{contentTitle:()=>i,default:()=>c,frontMatter:()=>o,metadata:()=>l,toc:()=>s});var a=n(87462),r=(n(67294),n(3905));const o={},i="Development and Contribution Info",l={type:"mdx",permalink:"/GreenTea/DEVELOPMENT",source:"@site/pages/DEVELOPMENT.md",title:"Development and Contribution Info",description:"Workspace Setup",frontMatter:{}},s=[{value:"Workspace Setup",id:"workspace-setup",level:2},{value:"Upgrading",id:"upgrading",level:2},{value:"Major-minor Versions",id:"major-minor-versions",level:3},{value:"Major-major Versions",id:"major-major-versions",level:2},{value:"Wally",id:"wally",level:2},{value:"Tests",id:"tests",level:2},{value:"Luau Rules",id:"luau-rules",level:2},{value:"Doc Comments",id:"doc-comments",level:2}],p={toc:s},u="wrapper";function c(e){let{components:t,...n}=e;return(0,r.kt)(u,(0,a.Z)({},p,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"development-and-contribution-info"},"Development and Contribution Info"),(0,r.kt)("h2",{id:"workspace-setup"},"Workspace Setup"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"This project uses aftman to manage tooling."),(0,r.kt)("li",{parentName:"ul"},"This project uses rojo to build and test the library."),(0,r.kt)("li",{parentName:"ul"},"This project uses lune to run development scripts."),(0,r.kt)("li",{parentName:"ul"},"This project uses wally to manage dependencies, plus wally-package-types and wally-patch-package.")),(0,r.kt)("h2",{id:"upgrading"},"Upgrading"),(0,r.kt)("p",null,"Ideally all major versions of GreenTea should use the same internal objects so that\n",(0,r.kt)("inlineCode",{parentName:"p"},"GreenTea.isGreenTeaType")," is true across all objects. This creates a splot between major versions:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},'"major-major" versions: these have breaking changes that don\'t allow us to unify the objects types'),(0,r.kt)("li",{parentName:"ul"},'"major-minor" versions: these have breaking changes that allow us to unify the objects types')),(0,r.kt)("h3",{id:"major-minor-versions"},"Major-minor Versions"),(0,r.kt)("p",null,"We can use a ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/dtolnay/semver-trick"},"semver-trick")," to help unify internal objects."),(0,r.kt)("p",null,"For example, say we want to change ",(0,r.kt)("inlineCode",{parentName:"p"},'GreenTea.string({ graphemes = "..." })')," from not requiring ",(0,r.kt)("inlineCode",{parentName:"p"},"bytes")," to be defined to requiring it to be defined (as it is now):"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"v1.0.0")," ",(0,r.kt)("em",{parentName:"li"},"does not")," require the user to specify ",(0,r.kt)("inlineCode",{parentName:"li"},"bytes"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"v2.0.0")," ",(0,r.kt)("em",{parentName:"li"},"does")," require the user to specify ",(0,r.kt)("inlineCode",{parentName:"li"},"bytes"),"."),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"v1.0.1")," takes ",(0,r.kt)("inlineCode",{parentName:"li"},"V2.0.0")," as a dependency, and returns a copy of ",(0,r.kt)("inlineCode",{parentName:"li"},"v2.0.0")," with ",(0,r.kt)("inlineCode",{parentName:"li"},"GreenTea.string")," modified to\ninclude a default value for ",(0,r.kt)("inlineCode",{parentName:"li"},"bytes")," if it is not specified.")),(0,r.kt)("p",null,"In this manner, ",(0,r.kt)("inlineCode",{parentName:"p"},"v1.0.1"),"'s internal objects are exactly the same as ",(0,r.kt)("inlineCode",{parentName:"p"},"v2.0.0"),"'s, and they'll be\n",(0,r.kt)("inlineCode",{parentName:"p"},"GreenTea.isGreenTeaType")," compatible."),(0,r.kt)("h2",{id:"major-major-versions"},"Major-major Versions"),(0,r.kt)("p",null,"For these, it's mostly like a typical major upgrade, except we should make ",(0,r.kt)("inlineCode",{parentName:"p"},"GreenTea.typeof")," take in previous versions' types and convert them. This gives an option for libraries expecting GreenTea types\nto compose with libraries that use previous versions."),(0,r.kt)("p",null,"Ideally, we should also do the inverse: make the previous version able to take in a newer GreenTea's Type and compose with it."),(0,r.kt)("h2",{id:"wally"},"Wally"),(0,r.kt)("p",null,"Wally is used to manage dependencies, but we also need types and patches."),(0,r.kt)("p",null,"The easiest way to do this is to run the lune script: ",(0,r.kt)("inlineCode",{parentName:"p"},"lune run wally-install")," .\nAlternatively, go read the lune script and see what it does."),(0,r.kt)("p",null,"We only use package patches to fix some output issues with jest-lua."),(0,r.kt)("h2",{id:"tests"},"Tests"),(0,r.kt)("p",null,"Tests are ran using jest-lua. These require a fflag to be set."),(0,r.kt)("p",null,"A Lune script exists to set this flag: ",(0,r.kt)("inlineCode",{parentName:"p"},"lune run enable-loadmodule")," . This..."),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"Finds the Roblox install directory, and the Roblox Studio's version folder"),(0,r.kt)("li",{parentName:"ul"},"Finds or creates ",(0,r.kt)("inlineCode",{parentName:"li"},"ClientSettings/ClientAppSettings.json")),(0,r.kt)("li",{parentName:"ul"},"Sets the flag in that file"),(0,r.kt)("li",{parentName:"ul"},"Sets the file to read-only, so Roblox doesn't overwrite it on startup.")),(0,r.kt)("p",null,"This is only implemented for Windows, because I only have a Windows machine.\nIf you have a Mac feel free to implement this for your platform in the lune script."),(0,r.kt)("h2",{id:"luau-rules"},"Luau Rules"),(0,r.kt)("p",null,"We disable LocalShadow because it's the only way to redefine a variable's types.\nWe'd like to remove this when it's not longer necessary."),(0,r.kt)("h2",{id:"doc-comments"},"Doc Comments"),(0,r.kt)("p",null,"Doc comments should be compatible with both moonwave and with luau-lsp."),(0,r.kt)("p",null,"This poses a problem for newlines: luau-lsp only works with ",(0,r.kt)("inlineCode",{parentName:"p"},"\\")," for newlines, but moonwave only works with double-spaces for newlines."),(0,r.kt)("p",null,"For now, we combine these two. This makes the moonwave docs look funny, because it won't omit the ",(0,r.kt)("inlineCode",{parentName:"p"},"\\")," before newlines, but \ud83e\udd37\u200d\u2640\ufe0f what can you do."))}c.isMDXComponent=!0}}]);