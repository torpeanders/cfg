-- Bootstrap lazy.nvim (plugin manager)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({"git", "clone", "--filter=blob:none", "https://github.com/folke/lazy.nvim.git", "--branch=stable", lazypath})
end
vim.opt.rtp:prepend(lazypath)

-- Plugin Setup
require("lazy").setup({
  "nvim-lua/plenary.nvim",         -- Dependency for other plugins
  "nvim-telescope/telescope.nvim", -- Fuzzy finder replacement
  "lewis6991/gitsigns.nvim",       -- Git integration
  "tpope/vim-fugitive",            -- Git wrapper
  "nvim-treesitter/nvim-treesitter", -- Better syntax highlighting
  "neovim/nvim-lspconfig",         -- Built-in LSP support
  "hrsh7th/nvim-cmp",              -- Autocompletion
  "hrsh7th/cmp-nvim-lsp",          -- LSP completion
  "L3MON4D3/LuaSnip",              -- Snippets
  "itchyny/lightline.vim"           -- Status line
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
vim.g.mapleader = ","

-- Keybindings
vim.keymap.set("n", "<leader>.", ":lcd %:p:h<CR>", { noremap = true, silent = true })

-- Window Navigation
vim.keymap.set("n", "<leader>h", "<C-w>h", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>l", "<C-w>l", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>j", "<C-w>j", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>k", "<C-w>k", { noremap = true, silent = true })

-- LSP Configuration
local lspconfig = require("lspconfig")
lspconfig.ts_ls.setup{}
lspconfig.pyright.setup{}

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
require("telescope").setup{}
vim.keymap.set("n", "<leader>ff", ":Telescope find_files<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fg", ":Telescope live_grep<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fb", ":Telescope buffers<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fh", ":Telescope help_tags<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fl", ":Telescope current_buffer_fuzzy_find<CR>", { noremap = true, silent = true })

-- Git Signs
require("gitsigns").setup()
