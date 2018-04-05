/**
 * @fileOverview
 * This file deals with filesystems panes.  It implements dynamic tabs on top
 * if Bootstrap.
 *
 * @version 0.2.0
 * @author Jan Wielemaker, J.Wielemaker@vu.nl
 * @requires jquery
 */

define([ "jquery", "config", "preferences", "cm/lib/codemirror",
	//  "laconic", "editor"
       ],
       function($) {

var filesystems = {
  
};


(function ($) {
  var pluginName = 'filesystems';
  var tabid = 0;

  /** @lends $.fn.filesystems */
  var methods = {
    /**
     * Turn the current element into a Bootstrap filesystems pane. All
     * children of the current element are changed into tabs.  The
     * child can control the mapping using:
     *
     *   - `data-label = "Label"`
     *   - `data-close = "disabled"`
     */
    _init: function(options) {
      options = options||{};

           },


    navContent: function() {


      return this.find("div.tab-content").first();
    }
  }; // methods


  function genId()
  { return "filesystems-tab-"+tabid++;
  }


  /**
   * <Class description>
   *
   * @class filesystems
   * @tutorial jquery-doc
   * @memberOf $.fn
   * @param {String|Object} [method] Either a method name or the jQuery
   * plugin initialization object.
   * @param [...] Zero or more arguments passed to the jQuery `method`
   */

  $.fn.filesystems = function(method) {
    if ( methods[method] ) {
      return methods[method]
	.apply(this, Array.prototype.slice.call(arguments, 1));
    } else if ( typeof method === 'object' || !method ) {
      return methods._init.apply(this, arguments);
    } else {
      $.error('Method ' + method + ' does not exist on jQuery.' + pluginName);
    }
  };
})(jQuery);

  return filesystems;
});












