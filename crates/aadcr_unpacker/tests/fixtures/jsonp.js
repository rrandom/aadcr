(self.webpackChunk_N_E = self.webpackChunk_N_E || []).push([
  [1234], // Chunk ID
  { // Module definitions...
    // Module 1: Exporting a utility function using `module.exports`
    1111: function (module, exports, require) {
      // Utility function defined in Module 1
      function utilityFunction() {
        console.log("Utility function from Module 1.");
      }

      // Exporting the utility function as the module's public interface
      module.exports = utilityFunction;
    },

    // Module 2: Using `exports` to export functions and `require` to import from Module 1
    2222: function (module, exports, require) {
      // Requiring the utility function from Module 1
      var utilityFunction = require(1111); // Using the module ID to require Module 1

      // Function that uses the imported utilityFunction
      function functionUsingUtility() {
        console.log("Function in Module 2 calling utility function:");
        utilityFunction(); // Using the utility function from Module 1
      }

      // Another standalone function in Module 2
      function standaloneFunction() {
        console.log("Standalone function in Module 2.");
      }

      // Exporting both functions as properties of the exports object
      exports.functionUsingUtility = functionUsingUtility;
      exports.standaloneFunction = standaloneFunction;
    },
  },
  function (module) {
    // Initialization function: Sets up the chunk and initializes its modules.

    // 'init' function: Loads and executes a specific module by its ID.
    var init = function (moduleId) {
      // Sets the current module ID ('module.s') and loads the module.
      return module((module.s = moduleId));
    };

    // 'module.O' method: Part of webpack's runtime, handling module and chunk loading.
    // First Call to 'module.O': Handles loading of dependent chunks.
    module.O(0, [9774, 179], function () {
      // This callback is executed once all dependent chunks (9774, 179) are loaded.
      // Dependent chunks are other chunks that this chunk needs before it can be initialized.
      // For example, these could be vendor chunks or previously split code chunks that contain shared libraries or components.
    }).then(function () {
      // Second Call to 'module.O': Initializes the entry modules of this chunk.
      // Once dependent chunks are loaded, we proceed to initialize this chunk's entry modules (54885, 11867).
      // These module IDs represent the starting points or root modules of this chunk, which kickstart the functionality encapsulated within the chunk.
      module.O(0, [], function () {
        return init(54885), init(11867);
      }, 1); // The '1' here might indicate a different operation type, such as initializing entry modules.

      // '_N_E' assignment: Marks the chunk as successfully initialized.
      // 'module.O()' returns a promise that resolves when the chunk is fully loaded and initialized.
      // This assignment can be used for debugging or for chaining further actions once initialization is complete.
      (_N_E = module.O());
    });
  },
]);