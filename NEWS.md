Version 1.6.0   - Under development
  - Improvement of <f:viewParam> to accept a from EL expression to setup the value
  - Add util:parseJSON() EL function
  - Integrate jQuery UI 1.13.2
  - Fix #4: Support to build with -gnatW8

Version 1.5.0   - Aug 2022
  - New widget <w:progress> to display horizontal/vertical progress bars

Version 1.4.3   - Jul 2021
  - Add jQuery 3.6.0
  - Add a programmer's guide
  - Remove very old jQuery 1.11.3, jQuery UI 1.11.4, jQuery Chosen 1.4.2

Version 1.4.2   - Feb 2021
  - Fix compilation warnings
  - Cleanup build and examples

Version 1.4.1   - Nov 2020
  - Fix translations, compilation warnings

Version 1.4.0   - May 2020
  - Performance improvement for the Facelet cache
  - Integrate jQuery 3.4.1, jQuery UI 1.12.1, jQuery Chosen 1.8.7
  - New <f:validateRegex> to validate an input field with a regular expression

Version 1.3     - Dec 2019
  - New converter to format floating point numbers
  - Improvement of the build environment

Version 1.2     - Jul 2018
  - New EL function util:translate for translation with a resource bundle
  - New REST servlet with support for server API implementation
  - Provide pre-defined beans in ASF contexts: requestScope
  - Add support for servlet requests to retrieve the body content as a stream
  - Improvement of navigation rules to allow setting the return status
  - Moved the servlet support in a separate project: ada-servlet
  - Integrate jQuery datetime picker

Version 1.1     - Dec 2015
  - New EL function util:formatDate
  - New request route mapping with support for URL component extraction and parameter
    injection in Ada beans
  - Improvement of configure, build and installation with gprinstall when available
  - Integrate jQuery 1.11.3 and jQuery UI 1.11.4
  - Integrate jQuery Chosen 1.4.2
  - New component <w:chosen> for the Chosen support
  - Added a servlet cache control filter

Version 1.0.1   - Jul 2014
  - Fix minor configuration issue with GNAT 2014
  - Fix concurrent issues in facelet and session cache implementation

Version 1.0     - Apr 2014
  - Add support for Facebook and Google+ login
  - Javascript support for popup and editable fields
  - Added support to enable/disable mouseover effect in lists
  - New EL function util:iso8601
  - New component <w:autocomplete> for input text with autocompletion
  - New component <w:gravatar> to render a gravatar image
  - New component <w:like> to render a Facebook, Twitter or Google+ like button
  - New component <w:panel> to provide collapsible div panels
  - New components <w:tabView> and <w:tab> for tabs display
  - New component <w:accordion> to display accordion tabs
  - Add support for JSF <f:facet>, <f:convertDateTime>, <h:doctype>
  - Support for the creation of Debian packages

Version 0.5     - Feb 2013
  - Moved the Security packages in a separate project: Ada Security
  - New demo to show OAuth and Facebook API integration
  - Integrated jQuery 1.8.3 and jQuery UI 1.9.2
  - New converter to display file sizes
  - Javascript support for click-to-edit behavior
  - Add support for JSF session beans
  - Add support for servlet error page customization
  - Allow navigation rules to handle exceptions raised by Ada bean actions
  - Support the JSF 2.2 conditional navigation
  - New functions fn:escapeXml and fn:replace

Version 0.4     - May 2012
  - Support for shared or static build configuration
  - Support for file upload
  - New component <h:inputFile>, <f:metadata>, <f:viewParam>, <f:viewAction>
  - New function util:hasMessage
  - Implement JSF phase events and phase listeners
  - Implement JSF/RoR flash context
  - Add pre-defined JSF beans: initParam, flash
  - Support for locales and honor the Accept-Language
  - Demos available in French and English

Version 0.3     - Feb 2012
  - Add documentation and examples for the supported tags
  - New components <h:inputTextarea>, <h:selectOneMenu>,
    <f:selectItem>, <f:selectItems>, <h:selectBooleanCheckbox>, <h:outputLabel>,
    <h:inputHidden>, <h:body>, <h:head>, <h:selectOneRadio>
  - New functions fn:substring, fn:indexOf, fn:trim, fn:contains,
    fn:startsWith, fn:endsWith
  - Implement a JSF exception handler framework
  - New date converter to format dates in a localized format
  - New components <util:escape>, <util:script>, <util:flush>,
    <util:set>, <util:file>
  - Provide pre-defined beans in ASF contexts: param, header
  - Add support for dialog boxes with jQuery UI

Version 0.2     - Sep 2011
  - Integrate jQuery UI 1.8.3 (MIT-License)
  - Implement JSF validation phase and validators
  - Add an AJAX servlet framework to invoke an action on a bean

Version 0.1     - May 2011
  - Base implementation of Java-like servlets, and JSF-like framework
