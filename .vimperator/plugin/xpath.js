/**
 * ==VimperatorPlugin==
 * @name    XPath Evaluator / Generator for Vimperator
 * @author  INA Lintaro
 * @version 0.01
 * ==/VimperatorPlugin==
 *
 * Usage:
 *
 * :xp[ath] [expression]
 *    Evaluate XPath expression and highlight elements expressed by the
 *    expression. Empty expression cancels previous highlights.
 *
 * :genxp[ath]
 * :gxp
 *    Generate XPath expression of an element on which mouseover event is
 *    detected. The generated expression will be insereted into the commandline
 *    as ':xpath expression'. Click somewhere to stop detection.
**/

(function(){
    var XPathHilight = {
        style: '2px solid red'
    };
    var XPathGenerator = {
        /* From jAutoPagerize. Simplified and fixed for Vimperator. */
        start: function() {
            var d = window.content.document;
            d.body.addEventListener('mouseover',
                                    XPathGenerator.mouseover, true);
            d.body.addEventListener('mousedown',
                                    XPathGenerator.mousedown, true);
            d.body.addEventListener('mouseout',
                                    XPathGenerator.mouseout, true);
        },
        stop: function() {
            var d = window.content.document;
            d.body.removeEventListener('mouseover',
                                       XPathGenerator.mouseover, true);
            d.body.removeEventListener('mousedown',
                                       XPathGenerator.mousedown, true);
            d.body.removeEventListener('mouseout',
                                       XPathGenerator.mouseout, true);
        },
        mouseover: function(e) {
            e.target.style.outline = XPathHilight.style;
            var cmd = 'xpath ' + XPathGenerator.getXPathByElement(e.target);
            liberator.modules.commandline.open(
                ':',
                cmd,
                liberator.modules.modes.EX);
        },
        mousedown: function(e) {
            e.target.style.outline = '';
            XPathGenerator.stop();
        },
        mouseout: function(e) {
            e.target.style.outline = '';
        },
        getXPathByElement: function(target) {
            function indexOf(node) {
                for (var i=0,r=1,c=node.parentNode.childNodes,len=c.length;
                     i < len; i++) {
                    if (c[i].nodeName == node.nodeName &&
                        c[i].nodeType == node.nodeType) {
                        if (c[i] == node) return r;
                        r++;
                    }
                }
                return -1;
            }

            var pathElement = '';
            var node = target;
            if (node.nodeType == 9 /*DOCUMENT_NODE=9*/) {
                return '';
            } else {
                var tagName = node.tagName.toLowerCase();
                if (node.hasAttribute('id')) {
                    pathElement='id("'+node.getAttribute("id")+'")';
                } else {
                    pathElement=arguments.callee(node.parentNode)+'/'+tagName;
                    if (node.hasAttribute("class")) {
                        pathElement
                            += '[@class="'+node.getAttribute("class")+'"]';
                    } else {
                        pathElement += '['+indexOf(node)+']';
                    }
                }
            }
            return pathElement;
        }
    };

    liberator.modules.commands.addUserCommand(
        ['xp[ath]'],
        "Evaluate xpath and hilight element",
        function (args) {
            if (XPathHilight.prev) {
                XPathHilight.prev.forEach(function(v,i) {
                    v.style.outline = XPathHilight.prevStyleOutline[i];
                });
            }
            XPathHilight.prev = [];
            XPathHilight.prevStyleOutline = [];
            var ret = liberator.modules.buffer.evaluateXPath(args);
            for (var i=0; i < ret.snapshotLength; i++) {
                var item = ret.snapshotItem(i);
                if (item) {
                    XPathHilight.prev.push(item);
                    XPathHilight.prevStyleOutline.push(item.style.outline);
                    item.style.outline = XPathHilight.style;
                }
            }
        });

    liberator.modules.commands.addUserCommand(
        ['genxp[ath]', 'gxp'],
        "Generate XPath by mouse hover",
        function () {
            XPathGenerator.start();
        });
})();
