/* samples -- Specific functions for the samples
 *  Copyright (C) 2012 Stephane Carrez
 *  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
function Sample_Highlight(name, keywords) {
    PR['registerLangHandler'](PR['sourceDecorator']({
        'keywords': (keywords),
        'types': /^(bool|(double|s?fixed|[su]?int)(32|64)|float|string)\b/,
        'cStyleComments': false
      }), [name]);
    prettyPrint();
}

/**
 * @brief Split long lines.
 *
 * @param text the text to split.
 * @return the text with long lines split.
 */
function splitLongLines(text) {
    var lines = text.split('\n');
    var i, j, txt = '';

    for (i = 0; i < lines.length; i++) {
        if (lines[i].length < 120) {
            txt = txt + '\n' + lines[i];
        } else {
            var l = lines[i].match(/.{1,120}/g);
            for (j = 0; j < l.length; j++) {
                txt = txt + '\n' + l[j];
            }
        }
    }
    return txt;
}

/**
 * @brief Reformat the XML.
 *
 * @param xml the XML to reformat.
 * @return the re-indended XML.
 */
function formatXml(xml) {
    var formatted = '';
    var reg = /(>)(<)(\/*)/g;
    xml = xml.replace(reg, '$1\r\n$2$3');
    var pad = 0;
    jQuery.each(xml.split('\r\n'), function(index, node) {
        var indent = 0;
        if (node.match( /.+<\/\w[^>]*>$/ )) {
            indent = 0;
        } else if (node.match( /^<\/\w/ )) {
            if (pad != 0) {
                pad -= 1;
            }
        } else if (node.match( /^<\w[^>]*[^\/]>.*$/ )) {
            indent = 1;
        } else {
            indent = 0;
        }

        var padding = '';
        for (var i = 0; i < pad; i++) {
            padding += '  ';
        }

        formatted += padding + node + '\r\n';
        pad += indent;
    });

    return formatted;
}