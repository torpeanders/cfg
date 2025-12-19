-- Bootstrap lazy.nvim (plugin manager)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({"git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazypath})
end
vim.opt.rtp:prepend(lazypath)

-- Plugin Setup
require("lazy").setup({
  "nvim-lua/plenary.nvim",                    -- Dependency for other plugins
  "nvim-telescope/telescope.nvim",            -- Fuzzy finder replacement
  { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
  "lewis6991/gitsigns.nvim",                  -- Git integration
  "tpope/vim-fugitive",                       -- Git wrapper
  "nvim-treesitter/nvim-treesitter",          -- Better syntax highlighting
  "neovim/nvim-lspconfig",                    -- Built-in LSP support
  "hrsh7th/nvim-cmp",                         -- Autocompletion
  "hrsh7th/cmp-nvim-lsp",                     -- LSP completion
  "L3MON4D3/LuaSnip",                         -- Snippets
  "itchyny/lightline.vim",                    -- Status line
  "folke/flash.nvim",
  "folke/which-key.nvim",                     -- Keybinding helper
  "morhetz/gruvbox",                          -- Gruvbox colorscheme
})

-- General Settings
vim.opt.breakindent = true
vim.opt.autoindent = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.background = "dark"
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.termguicolors = true
vim.g.mapleader = " "

-- Keybindings
vim.keymap.set("n", "<leader>.", ":lcd %:p:h<CR>", { noremap = true, silent = true })

-- Window Navigation
vim.keymap.set("n", "<leader>wh", "<C-w>h", { noremap = true, silent = true, desc = "Navigate window left" })
vim.keymap.set("n", "<leader>wl", "<C-w>l", { noremap = true, silent = true, desc = "Navigate window right" })
vim.keymap.set("n", "<leader>wj", "<C-w>j", { noremap = true, silent = true, desc = "Navigate window down" })
vim.keymap.set("n", "<leader>wk", "<C-w>k", { noremap = true, silent = true, desc = "Navigate window up" })

-- LSP Configuration
vim.lsp.enable('pyright')

-- Autocompletion Setup
local cmp = require("cmp")
cmp.setup({
  mapping = cmp.mapping.preset.insert({
    ["<Tab>"] = cmp.mapping.select_next_item(),
    ["<S-Tab>"] = cmp.mapping.select_prev_item(),
    ["<CR>"] = cmp.mapping.confirm({ select = true }),
  }),
  sources = cmp.config.sources({
    { name = "nvim_lsp" },
    { name = "luasnip" },
  })
})

-- Telescope Setup
require('telescope').setup {
  extensions = {
    fzf = {
      fuzzy = true,                    -- false will only do exact matching
      override_generic_sorter = true,  -- override the generic sorter
      override_file_sorter = true,     -- override the file sorter
      case_mode = "ignore_case",        -- or "ignore_case" or "respect_case"
                                       -- the default case_mode is "smart_case"
    }
  }
}
require('telescope').load_extension('fzf')

vim.keymap.set("n", "<leader>pf", ":Telescope find_files<CR>", { noremap = true, silent = true, desc = "Find file in project" })
vim.keymap.set("n", "<leader>sp", ":Telescope live_grep<CR>", { noremap = true, silent = true, desc = "Search (grep) in project" })
vim.keymap.set("n", "<leader>ss", ":Telescope current_buffer_fuzzy_find<CR>", { noremap = true, silent = true, desc = "Search in current buffer" })
vim.keymap.set("n", "<leader>bb", ":Telescope buffers<CR>", { noremap = true, silent = true, desc = "List open buffers" })
vim.keymap.set("n", "<leader>fr", function() require("telescope.builtin").oldfiles() end, { noremap = true, silent = true, desc = "Open recent files" })
vim.keymap.set("n", "<leader>sP", function() local word = vim.fn.expand("<cword>"); require("telescope.builtin").live_grep({ default_text = word }) end, { noremap = true, silent = true, desc = "Search symbol under cursor (live_grep)" })

-- Git Signs
require("gitsigns").setup({
  on_attach = function(bufnr)
    local gs = package.loaded.gitsigns

    -- Navigation
    vim.keymap.set("n", "]c", function() gs.next_hunk() end, { buffer = bufnr, desc = "Next Hunk" })
    vim.keymap.set("n", "[c", function() gs.prev_hunk() end, { buffer = bufnr, desc = "Prev Hunk" })

    -- Actions
    vim.keymap.set("n", "<leader>gs", function() gs.stage_hunk() end, { buffer = bufnr, desc = "Stage Hunk" })
    vim.keymap.set("n", "<leader>gu", function() gs.undo_stage_hunk() end, { buffer = bufnr, desc = "Undo Stage Hunk" })
    vim.keymap.set("n", "<leader>gr", function() gs.reset_hunk() end, { buffer = bufnr, desc = "Reset Hunk" })
    vim.keymap.set("n", "<leader>gR", function() gs.reset_buffer() end, { buffer = bufnr, desc = "Reset Buffer" })
    vim.keymap.set("n", "<leader>gp", function() gs.preview_hunk() end, { buffer = bufnr, desc = "Preview Hunk" })
    vim.keymap.set("n", "<leader>gb", function() gs.blame_line() end, { buffer = bufnr, desc = "Blame Line" })
  end
})

-- Flash
require("flash").setup()
vim.keymap.set("n", "<leader>jj", function() require("flash").jump() end, { desc = "Jump anywhere (char)" })
vim.keymap.set("n", "<leader>jl", function() require("flash").jump({ pattern = "^" }) end, { desc = "Jump to line" })

-- Which Key
require("which-key").setup({})

-- Color Theme
vim.cmd.colorscheme("gruvbox")

