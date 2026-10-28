
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(DT)
library(lubridate)
library(zoo)
library(tidyr)
library(forecast)

source("data_prep.R")
source("analysis_trend.R")
source("analysis_revenue.R")
source("analysis_backlog.R")
source("analysis_policy.R")
source("analysis_forecast.R")
source("analysis_correlation.R")

# ── Load data ────────────────────────────────────────────────
df      <- load_and_prepare("echallan_daily_data.csv")
monthly <- monthly_summary(df)
yearly  <- yearly_summary(df)

# ── Formatters ───────────────────────────────────────────────
fmt_n <- function(x) {
  x <- as.numeric(x)
  if (is.na(x)) return("N/A")
  if (x >= 1e7) return(paste0(round(x / 1e7, 1), " Cr"))
  if (x >= 1e5) return(paste0(round(x / 1e5, 1), " L"))
  if (x >= 1e3) return(paste0(round(x / 1e3, 1), " K"))
  as.character(round(x))
}
fmt_cr  <- function(x) paste0("Rs ", round(as.numeric(x) / 1e7, 1), " Cr")
fmt_pct <- function(x) paste0(round(as.numeric(x), 1), "%")
fmt_lakh <- function(x) {
  x <- as.numeric(x)
  if (is.na(x) || x == 0) return("0")
  if (x >= 1e7) return(paste0(round(x / 1e7, 2), " Cr"))
  if (x >= 1e5) return(paste0(round(x / 1e5, 2), " L"))
  if (x >= 1e3) return(paste0(round(x / 1e3, 1), " K"))
  as.character(round(x))
}

# ── Dark plotly theme ────────────────────────────────────────
BG   <- "#0b1622"
BOX  <- "#111f2e"
BOX2 <- "#152030"
GRID <- "#1c2f42"
FONT <- "#8daabf"
ACC  <- "#38bdf8"

dk <- function(p, title_x = NULL, title_y = NULL) {
  p %>% layout(
    paper_bgcolor = BOX,
    plot_bgcolor  = BOX,
    font   = list(color = FONT, family = "Inter, Arial, sans-serif", size = 12),
    xaxis  = list(
      title     = list(text = title_x, font = list(color = FONT, size = 12)),
      gridcolor = GRID, zerolinecolor = GRID,
      tickfont  = list(color = FONT)
    ),
    yaxis  = list(
      title     = list(text = title_y, font = list(color = FONT, size = 12)),
      gridcolor = GRID, zerolinecolor = GRID,
      tickfont  = list(color = FONT)
    ),
    legend = list(
      orientation = "h", y = -0.25, x = 0.5, xanchor = "center",
      bgcolor = "rgba(0,0,0,0)",
      font    = list(color = FONT, size = 11)
    ),
    margin = list(t = 10, b = 55, l = 60, r = 20),
    hoverlabel = list(
      bgcolor    = "#1c3248",
      bordercolor = ACC,
      font = list(color = "#ffffff", size = 12)
    )
  )
}

# ── UI helpers ───────────────────────────────────────────────
desc_box <- function(icon_name, title, text) {
  tags$div(class = "desc-box",
    tags$div(class = "desc-icon", icon(icon_name)),
    tags$div(
      tags$div(class = "desc-title", title),
      tags$div(class = "desc-text",  text)
    )
  )
}

findings_box <- function(...) {
  tags$div(class = "findings-box",
    tags$div(class = "findings-title",
      icon("lightbulb"), " Key Findings & Interpretation"
    ),
    tags$div(class = "findings-body", ...)
  )
}

finding_item <- function(label, text, color = "#34d399") {
  tags$div(class = "finding-item",
    tags$span(class = "finding-label", style = paste0("color:", color, ";"), label),
    tags$span(class = "finding-text", text)
  )
}

kpi_card <- function(value, label, icon_name, color = ACC) {
  tags$div(class = "kpi-card",
    tags$div(class = "kpi-icon",  icon(icon_name)),
    tags$div(class = "kpi-value", style = paste0("color:", color, ";"), value),
    tags$div(class = "kpi-label", label)
  )
}

# Pure CSS India flag — no external URL needed
india_flag <- tags$span(
  style = paste0(
    "display:inline-flex;align-items:center;gap:0px;",
    "vertical-align:middle;margin-right:8px;",
    "border-radius:2px;overflow:hidden;width:22px;height:14px;flex-shrink:0;"
  ),
  tags$span(style = "flex:1;background:#FF9933;height:100%;"),
  tags$span(
    style = "flex:1;background:#FFFFFF;height:100%;display:flex;align-items:center;justify-content:center;",
    tags$span(style = "width:6px;height:6px;border-radius:50%;border:1.5px solid #000080;")
  ),
  tags$span(style = "flex:1;background:#138808;height:100%;")
)

# ── CSS ──────────────────────────────────────────────────────
css <- HTML(paste0("
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap');

body, .wrapper {
  font-family: 'Inter', sans-serif !important;
}


.main-sidebar { background: #070e1a !important; border-right: 1px solid #14243a !important; }
.main-header .logo, .main-header .navbar {
  background: #070e1a !important; border-bottom: 1px solid #14243a !important;
}
.main-header .logo {
  color: ", ACC, " !important; font-weight: 800 !important; font-size: 14px !important;
  display: flex !important; align-items: center !important;
}
.sidebar-menu > li > a {
  color: #4a6a84 !important; font-size: 13px; padding: 12px 18px;
  transition: all .2s ease; border-left: 3px solid transparent;
}
.sidebar-menu > li > a:hover {
  background: #0e1e2e !important; color: ", ACC, " !important;
  border-left-color: ", ACC, " !important; padding-left: 22px !important;
}
.sidebar-menu > li.active > a {
  background: linear-gradient(90deg, #0d2236 0%, ", BG, " 100%) !important;
  color: ", ACC, " !important; border-left-color: ", ACC, " !important; font-weight: 600 !important;
}
.sidebar-menu > li > a .fa { color: ", ACC, " !important; margin-right: 10px; width: 16px; }
.sidebar-brand { padding: 18px 18px 14px; border-bottom: 1px solid #14243a; margin-bottom: 6px; }
.sidebar-brand .brand-title { color: ", ACC, "; font-weight: 800; font-size: 11px; letter-spacing: .7px; margin-bottom: 5px; }
.sidebar-brand .brand-sub { color: #2a4060; font-size: 11px; line-height: 1.75; }
.sidebar-footer { position: absolute; bottom: 0; width: 100%; padding: 12px 18px; border-top: 1px solid #14243a; }

.content-wrapper { background: ", BG, " !important; }
.content { padding: 20px 22px !important; }

.box {
  background: ", BOX, " !important; border: 1px solid #14243a !important;
  border-radius: 14px !important; box-shadow: 0 4px 20px rgba(0,0,0,.4) !important;
  margin-bottom: 18px;
}
.box-header {
  background: transparent !important; border-bottom: 1px solid #14243a !important;
  border-radius: 14px 14px 0 0 !important; padding: 14px 18px 12px !important;
}
.box-title { color: #c8dcea !important; font-size: 13px !important; font-weight: 600 !important; }
.box-body  { padding: 16px !important; }

.page-header { margin-bottom: 20px; padding-bottom: 14px; border-bottom: 1px solid #14243a; }
.page-header .accent-bar { width: 36px; height: 3px; background: linear-gradient(90deg, ", ACC, ", #6366f1); border-radius: 2px; margin-bottom: 10px; }
.page-header h2 { color: #e2edf5; font-weight: 800; font-size: 21px; margin: 0 0 4px; }
.page-header .page-sub { color: #3a5870; font-size: 12.5px; }

.desc-box {
  display: flex; gap: 14px; align-items: flex-start;
  background: #0c1c2c; border: 1px solid #14243a; border-left: 3px solid ", ACC, ";
  border-radius: 10px; padding: 14px 16px; margin-bottom: 16px;
}
.desc-icon  { color: ", ACC, "; font-size: 18px; margin-top: 2px; min-width: 20px; }
.desc-title { color: #c8dcea; font-weight: 700; font-size: 13px; margin-bottom: 5px; }
.desc-text  { color: #4a6a84; font-size: 12.5px; line-height: 1.8; }

.findings-box {
  background: #0a1a28; border: 1px solid #14243a; border-left: 3px solid #22c55e;
  border-radius: 10px; padding: 16px 18px; margin-top: 18px; margin-bottom: 6px;
}
.findings-title { color: #22c55e; font-weight: 700; font-size: 13px; margin-bottom: 12px; letter-spacing: .2px; }
.findings-body  { display: flex; flex-direction: column; gap: 9px; }
.finding-item   { display: flex; gap: 10px; align-items: flex-start; font-size: 12.5px; }
.finding-label  { font-weight: 700; white-space: nowrap; min-width: 165px; font-size: 12px; }
.finding-text   { color: #4a6a84; line-height: 1.7; }

.kpi-row  { display: flex; gap: 14px; margin-bottom: 18px; flex-wrap: wrap; }
.kpi-card {
  flex: 1; min-width: 155px; background: ", BOX2, "; border: 1px solid #14243a;
  border-radius: 12px; padding: 16px 18px; display: flex; flex-direction: column;
  gap: 6px; transition: transform .15s, box-shadow .15s;
}
.kpi-card:hover { transform: translateY(-2px); box-shadow: 0 8px 24px rgba(0,0,0,.35); }
.kpi-icon  { font-size: 20px; color: #213448; }
.kpi-value { font-size: 21px; font-weight: 800; line-height: 1; }
.kpi-label { font-size: 11px; color: #2d4a60; font-weight: 500; letter-spacing: .3px; }

.stat-chip {
  display: inline-block; background: #0c1c2c; border: 1px solid #14243a;
  border-radius: 6px; padding: 3px 10px; font-size: 11.5px; color: #4a6a84; margin: 2px;
}
.covid-note {
  display: inline-block; background: rgba(253,224,71,.1); border: 1px solid rgba(253,224,71,.2);
  border-radius: 6px; padding: 4px 10px; font-size: 11.5px; color: #fde047; margin-top: 4px;
}

.fc-legend {
  display: flex; gap: 18px; flex-wrap: wrap; margin-bottom: 12px;
  background: #0c1c2c; border-radius: 8px; padding: 10px 14px; border: 1px solid #14243a;
}
.fc-legend-item { display: flex; align-items: center; gap: 7px; font-size: 12px; color: #4a6a84; }
.fc-dot { width: 10px; height: 10px; border-radius: 50%; flex-shrink: 0; }

.acc-panel { background: #070e1a; border: 1px solid #14243a; border-radius: 10px; padding: 14px; margin-top: 14px; }
.acc-title { color: ", ACC, "; font-size: 12px; font-weight: 700; margin-bottom: 10px; }

.btn-run {
  background: linear-gradient(135deg, #1d4ed8, #0ea5e9) !important;
  border: none !important; border-radius: 9px !important; font-weight: 700 !important;
  font-size: 13px !important; color: #fff !important; padding: 10px 0 !important;
  width: 100% !important; box-shadow: 0 4px 14px rgba(14,165,233,.25) !important;
  transition: opacity .2s !important;
}
.btn-run:hover { opacity: .85 !important; }

.selectize-input { background: #0c1c2c !important; color: #8daabf !important; border: 1px solid #1e3a52 !important; border-radius: 8px !important; }
.selectize-dropdown { background: #0c1c2c !important; color: #8daabf !important; border: 1px solid #1e3a52 !important; }
.selectize-dropdown .option:hover { background: #14243a !important; }
.irs--shiny .irs-bar, .irs--shiny .irs-handle { background: ", ACC, " !important; border-color: ", ACC, " !important; }
.irs--shiny .irs-single { background: #1d4ed8 !important; color: #fff !important; font-size: 11px !important; }
.irs--shiny .irs-min, .irs--shiny .irs-max { color: #2d4a60 !important; }

table.dataTable { background: ", BOX, " !important; color: #8daabf !important; border-collapse: collapse !important; font-size: 12.5px !important; }
table.dataTable thead th { background: #0c1c2c !important; color: ", ACC, " !important; border-bottom: 1px solid #14243a !important; font-weight: 600 !important; padding: 10px 14px !important; }
table.dataTable tbody td { border-top: 1px solid #131f2e !important; padding: 9px 14px !important; }
table.dataTable tbody tr:hover td { background: #14243a !important; }
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_length,
.dataTables_wrapper .dataTables_filter,
.dataTables_wrapper .dataTables_paginate { color: #2d4a60 !important; font-size: 12px !important; }

pre { background: #070e1a !important; color: ", ACC, " !important; border: 1px solid #14243a !important; border-radius: 8px !important; font-size: 12px !important; padding: 12px !important; }

::-webkit-scrollbar { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: #070e1a; }
::-webkit-scrollbar-thumb { background: #1a2e42; border-radius: 3px; }
"))

# ════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title = tags$span(
      style = "display:flex;align-items:center;color:#38bdf8;font-weight:800;font-size:14px;",
      india_flag,
      "E-Challan Analytics"
    ),
    titleWidth = 250
  ),

  dashboardSidebar(
    width = 250,
    tags$head(tags$style(css)),
    tags$div(class = "sidebar-brand",
      tags$div(class = "brand-title", "INTEGRATED ANALYSIS DASHBOARD"),
      tags$div(class = "brand-sub",
        "Indian E-Challan System", tags$br(),
        paste0("Data: 2015 \u2014 Dec 2025"), tags$br(),
        paste0("Records up to: ", format(max(df$date), "%b %Y"))
      )
    ),
    sidebarMenu(id = "tabs",
      menuItem("Trend Analysis",       tabName = "trends",      icon = icon("chart-line")),
      menuItem("Revenue & Efficiency", tabName = "revenue",     icon = icon("rupee-sign")),
      menuItem("Backlog & Court",      tabName = "backlog",     icon = icon("balance-scale")),
      menuItem("MV Act 2019 Impact",   tabName = "policy",      icon = icon("gavel")),
      menuItem("Forecasting",          tabName = "forecast",    icon = icon("chart-area")),
      menuItem("Correlation Analysis", tabName = "correlation", icon = icon("project-diagram"))
    ),
    tags$div(class = "sidebar-footer",
      tags$div(style = "color:#2d4a60;font-size:11px;line-height:1.85;",
        tags$span(style = "color:#22c55e;margin-right:5px;", "\u25cf"), "Dashboard Active", tags$br(),
        tags$span(style = "font-size:9px;margin-right:5px;", "\u25cb"), "Hover charts for details"
      )
    )
  ),

  dashboardBody(
    tabItems(

      # ════════════════════════════════════════════════════
      # TAB 1 — TREND ANALYSIS
      # ════════════════════════════════════════════════════
      tabItem(tabName = "trends",
        tags$div(class = "page-header",
          tags$div(class = "accent-bar"),
          tags$h2("Trend Analysis"),
          tags$div(class = "page-sub",
                   "Year-by-year evolution of enforcement activity, revenue and compliance (2015\u20132025)")
        ),
        desc_box("chart-line", "About This Analysis",
          "This section provides a longitudinal view of India\u2019s e-challan enforcement system over the full study period.
           Annual bar charts reveal absolute volumes while growth rate charts expose acceleration and deceleration in enforcement.
           The compliance vs court burden chart shows the relationship between fine payment behaviour and judicial load over time."
        ),
        tags$div(class = "kpi-row",
          kpi_card(fmt_n(sum(df$totalChallan, na.rm=TRUE)),   "Total Challans Issued",  "car",               ACC),
          kpi_card(fmt_cr(sum(df$totalAmount,  na.rm=TRUE)),  "Total Revenue",           "rupee-sign",        "#22c55e"),
          kpi_card(fmt_pct(sum(df$disposedChallan,na.rm=TRUE)/sum(df$totalChallan,na.rm=TRUE)*100),
                   "Overall Compliance Rate", "check-circle",      "#34d399"),
          kpi_card(fmt_pct(sum(df$pendingChallan,na.rm=TRUE)/sum(df$totalChallan,na.rm=TRUE)*100),
                   "Overall Backlog Rate",    "exclamation-circle","#f87171")
        ),
        fluidRow(
          box(width=6, solidHeader=TRUE, status="primary",
              title=tagList(icon("chart-bar")," Annual Challans Issued"),
              tags$p("Total traffic violation notices issued each year (in millions). Line shows overall trend.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("tr_challans",height=265)),
          box(width=6, solidHeader=TRUE, status="success",
              title=tagList(icon("rupee-sign")," Annual Revenue Collected (Rs Crore)"),
              tags$p("Total fine revenue collected from disposed challans per year.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("tr_revenue",height=265))
        ),
        fluidRow(
          box(width=6, solidHeader=TRUE, status="warning",
              title=tagList(icon("percent")," Compliance Rate vs Court Burden (%)"),
              tags$p("Green = compliance rate (higher is better). Orange = court burden rate (lower is better).",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("tr_rates",height=255)),
          box(width=6, solidHeader=TRUE, status="info",
              title=tagList(icon("arrow-up")," Year-on-Year Growth Rate (%)"),
              tags$p("Percentage change vs previous year. Green bars = growth. Red bars = decline.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("tr_growth",height=255))
        ),
        findings_box(
          finding_item("Enforcement Growth",   "Challans grew dramatically over 2015\u20132025, indicating major expansion of digital enforcement infrastructure across Indian states.", "#38bdf8"),
          finding_item("Revenue Trajectory",   "Revenue grew faster than challans after 2019, confirming the MV Amendment Act\u2019s higher fine amounts boosted per-challan revenue significantly.", "#22c55e"),
          finding_item("Compliance Trend",     "Compliance rate shifts reveal offender behaviour \u2014 the post-2019 spike reflects deterrence from higher fines, while COVID-period dips are lockdown artefacts.", "#fbbf24"),
          finding_item("Court Burden Rising",  "Rising court burden in post-2019 years confirms higher fines motivated more offenders to contest challans legally, increasing judicial load.", "#f97316")
        )
      ),

      # ════════════════════════════════════════════════════
      # TAB 2 — REVENUE & EFFICIENCY
      # ════════════════════════════════════════════════════
      tabItem(tabName = "revenue",
        tags$div(class="page-header",
          tags$div(class="accent-bar"),
          tags$h2("Revenue & Collection Efficiency"),
          tags$div(class="page-sub","Fine collection patterns, revenue gaps and efficiency metrics across the study period")
        ),
        desc_box("rupee-sign","About This Analysis",
          "Revenue efficiency analysis examines how effectively the e-challan system converts issued fines into actual collected revenue.
           The revenue gap tracks uncollected pending fines \u2014 a growing gap reveals systemic collection weaknesses.
           Average fine per challan directly measures the monetary impact of policy changes like the MV Act 2019.
           The collection rate trend shows whether the system is improving or deteriorating over time."
        ),
        tags$div(class="kpi-row",
          kpi_card(fmt_cr(sum(df$disposedAmount,na.rm=TRUE)), "Total Revenue Collected",  "check-circle",       "#22c55e"),
          kpi_card(fmt_cr(sum(df$pendingAmount, na.rm=TRUE)), "Total Revenue Pending",    "exclamation-circle", "#f87171"),
          kpi_card(paste0(round(sum(df$disposedAmount,na.rm=TRUE)/sum(df$totalAmount,na.rm=TRUE)*100,1),"%"),
                   "Overall Collection Rate","percent",ACC),
          kpi_card(paste0("Rs ",round(sum(df$totalAmount,na.rm=TRUE)/sum(df$totalChallan,na.rm=TRUE),0)),
                   "Avg Fine per Challan","coins","#c084fc")
        ),
        fluidRow(
          box(width=4,solidHeader=TRUE,status="success",
              title=tagList(icon("coins")," Avg Fine per Challan (Rs)"),
              tags$p("Monthly average fine per disposed challan. Rise = policy impact or higher-value violations enforced.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("rv_per",height=258)),
          box(width=4,solidHeader=TRUE,status="danger",
              title=tagList(icon("exclamation-circle")," Revenue Gap \u2014 Uncollected (Rs Crore)"),
              tags$p("Monthly pending revenue not yet collected. Growing gap = enforcement weakness.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("rv_gap",height=258)),
          box(width=4,solidHeader=TRUE,status="warning",
              title=tagList(icon("percent")," Fine Collection Rate (%)"),
              tags$p("% of total fines collected per month. Dashed = overall average.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("rv_rate",height=258))
        ),
        fluidRow(
          box(width=12,solidHeader=TRUE,status="primary",
              title=tagList(icon("layer-group")," Collected vs Pending Revenue Over Time (Rs Crore)"),
              tags$p("Green = collected. Red = pending. Stacked = full revenue potential of all challans issued.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("rv_stack",height=265))
        ),
        findings_box(
          finding_item("Average Fine Increase",  "Clear step-change in avg fine per challan after Oct 2019, directly attributable to the MV Amendment Act\u2019s higher penalty structure.", "#c084fc"),
          finding_item("Revenue Gap",            "The persistent and growing revenue gap indicates a large portion of fines remain uncollected, representing both fiscal loss and an enforcement challenge.", "#f87171"),
          finding_item("Collection Rate",        "Collection rate fluctuations reveal periods of enforcement drive and relaxation. A rate sustained below average signals systemic non-compliance.", "#fbbf24"),
          finding_item("Collected vs Pending",   "The widening gap between collected and pending revenue underscores the need for stronger follow-up mechanisms in the e-challan system.", "#38bdf8")
        )
      ),

      # ════════════════════════════════════════════════════
      # TAB 3 — BACKLOG & COURT
      # ════════════════════════════════════════════════════
      tabItem(tabName = "backlog",
        tags$div(class="page-header",
          tags$div(class="accent-bar"),
          tags$h2("Backlog & Court Burden Analysis"),
          tags$div(class="page-sub","Unresolved challan accumulation and judicial system pressure over time")
        ),
        desc_box("balance-scale","About This Analysis",
          "The backlog rate measures the proportion of issued challans that remain unresolved at any given time.
           A persistently high or growing backlog indicates the disposal system cannot keep pace with new issuances.
           The court burden rate captures judicial stress \u2014 when more challans are contested in court,
           the judiciary faces increasing pressure, delaying resolution and increasing costs for all parties."
        ),
        tags$div(class="kpi-row",
          kpi_card(fmt_n(sum(df$pendingChallan, na.rm=TRUE)),  "Total Pending Challans",  "hourglass-half",    "#f87171"),
          kpi_card(fmt_n(sum(df$disposedChallan, na.rm=TRUE)), "Total Disposed Challans", "check-double",      "#34d399"),
          kpi_card(paste0(round(sum(df$pendingCourt,na.rm=TRUE)/sum(df$totalCourt,na.rm=TRUE)*100,1),"%"),
                   "Overall Court Burden Rate","gavel","#f97316"),
          kpi_card(paste0(round(sum(df$pendingChallan,na.rm=TRUE)/sum(df$totalChallan,na.rm=TRUE)*100,1),"%"),
                   "Overall Backlog Rate","exclamation-triangle","#fbbf24")
        ),
        fluidRow(
          box(width=6,solidHeader=TRUE,status="warning",
              title=tagList(icon("exchange-alt")," Pending vs Disposed Challans (Monthly)"),
              tags$p("Red = unresolved challans. Green = resolved challans per month.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("bl_trend",height=285)),
          box(width=6,solidHeader=TRUE,status="danger",
              title=tagList(icon("balance-scale")," Court Burden Rate (%) Over Time"),
              tags$p("% of total court cases still awaiting judgment. Dashed = overall average.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("bl_court",height=285))
        ),
        fluidRow(
          box(width=12,solidHeader=TRUE,status="info",
              title=tagList(icon("table")," Yearly Backlog & Court Summary"),
              tags$p("Complete year-by-year breakdown. Compliance % in green. Backlog % in red. Court Burden % in orange.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              DTOutput("bl_table"))
        ),
        findings_box(
          finding_item("Backlog Accumulation", "Pending challans grew consistently, indicating disposal mechanisms have not scaled proportionally with enforcement activity.", "#f87171"),
          finding_item("Court Burden Post-2019","Court burden rose after the MV Act, confirming higher fines motivated offenders to contest challans legally rather than pay directly.", "#f97316"),
          finding_item("Disposal Gap",          "The persistent gap between pending and disposed challans reveals a structural imbalance: challans are issued faster than they are resolved.", "#fbbf24"),
          finding_item("Judicial Implications", "High court burden has compounding effects \u2014 it slows resolution for all cases, deepens the backlog further, and delays revenue collection.", "#38bdf8")
        )
      ),

      # ════════════════════════════════════════════════════
      # TAB 4 — MV ACT 2019
      # ════════════════════════════════════════════════════
      tabItem(tabName = "policy",
        tags$div(class="page-header",
          tags$div(class="accent-bar"),
          tags$h2("Motor Vehicles (Amendment) Act 2019 \u2014 Impact Analysis"),
          tags$div(class="page-sub","Statistical before-vs-after evaluation of key metrics following October 2019 enforcement")
        ),
        desc_box("gavel","About This Analysis",
          "The MV Amendment Act 2019 dramatically increased traffic fine amounts \u2014 drunk driving fines rose from Rs 2,000 to Rs 10,000 (5x).
           This tab uses the Welch two-sample t-test to statistically compare key metrics before and after October 2019.
           A p-value below 0.05 confirms the change is statistically significant and not due to random variation.
           The COVID-19 period (Mar 2020 \u2013 Jun 2021) is isolated separately to prevent lockdown disruptions
           from confounding the true policy effect."
        ),
        fluidRow(
          box(width=12,solidHeader=FALSE,
            tags$div(style="display:flex;gap:40px;flex-wrap:wrap;padding:4px 0;",
              tags$div(
                tags$div(style="color:#f97316;font-weight:700;font-size:12px;margin-bottom:8px;",icon("arrow-up")," KEY FINE INCREASES"),
                tags$div(style="line-height:2.2;",
                  tags$span(class="stat-chip","Drunk driving: Rs 2,000 \u2192 Rs 10,000"),
                  tags$span(class="stat-chip","Overspeeding: Rs 400 \u2192 Rs 2,000"),
                  tags$span(class="stat-chip","No helmet: Rs 100 \u2192 Rs 1,000"),
                  tags$span(class="stat-chip","No seatbelt: Rs 100 \u2192 Rs 1,000"),
                  tags$span(class="stat-chip","Mobile use: Rs 1,000 \u2192 Rs 5,000"),
                  tags$span(class="stat-chip","No licence: Rs 500 \u2192 Rs 5,000")
                )
              ),
              tags$div(
                tags$div(style="color:#38bdf8;font-weight:700;font-size:12px;margin-bottom:8px;",icon("info-circle")," TIMELINE"),
                tags$div(style="line-height:2.2;",
                  tags$span(class="stat-chip","Act Passed: September 2019"),
                  tags$span(class="stat-chip","Enforcement: October 2019"),tags$br(),
                  tags$div(class="covid-note",icon("virus")," COVID-19: Mar 2020 \u2013 Jun 2021 (highlighted on all charts)")
                )
              )
            )
          )
        ),
        tags$div(style="color:#8daabf;font-weight:700;font-size:11.5px;letter-spacing:.5px;margin:6px 0 10px;","CHALLANS \u2014 BEFORE vs AFTER OCT 2019"),
        tags$div(class="kpi-row",
          uiOutput("pol_cb_before"), uiOutput("pol_cb_after"),
          uiOutput("pol_cb_change"), uiOutput("pol_cb_sig")
        ),
        tags$div(style="color:#8daabf;font-weight:700;font-size:11.5px;letter-spacing:.5px;margin:6px 0 10px;","REVENUE \u2014 BEFORE vs AFTER OCT 2019"),
        tags$div(class="kpi-row",
          uiOutput("pol_rv_before"), uiOutput("pol_rv_after"),
          uiOutput("pol_rv_change"), uiOutput("pol_rv_sig")
        ),
        fluidRow(
          box(width=6,solidHeader=TRUE,status="primary",
              title=tagList(icon("chart-line")," Total Challans Over Time"),
              tags$p("Blue = Before MV Act. Teal = After. Red dashed = Oct 2019 cutoff. Yellow band = COVID-19.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("pol_challan",height=265)),
          box(width=6,solidHeader=TRUE,status="success",
              title=tagList(icon("rupee-sign")," Revenue Over Time"),
              tags$p("Revenue jump after the Act reflects significantly higher fine amounts per violation.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("pol_revenue",height=265))
        ),
        fluidRow(
          box(width=6,solidHeader=TRUE,status="warning",
              title=tagList(icon("check-circle")," Compliance Rate Before vs After"),
              tags$p("Did higher fines create a deterrence effect and improve compliance behaviour?",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("pol_compliance",height=248)),
          box(width=6,solidHeader=TRUE,status="danger",
              title=tagList(icon("gavel")," Backlog & Court Burden Before vs After"),
              tags$p("Did higher fines lead more offenders to contest challans in court?",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("pol_backlog",height=248))
        ),
        fluidRow(
          box(width=12,solidHeader=TRUE,status="info",
              title=tagList(icon("table")," Statistical Comparison Table (Welch t-test)"),
              tags$p("P-value < 0.05 = statistically significant. P shown as '< 0.0001' when extremely small to avoid displaying raw zero.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              DTOutput("pol_table"))
        ),
        findings_box(
          finding_item("Challans Surged",     "Dramatic increase in monthly challans post-Oct 2019 (p < 0.0001) reflects intensified enforcement activity following the Act\u2019s enactment.", "#38bdf8"),
          finding_item("Revenue Multiplied",  "Revenue growth far exceeded challan growth, confirming that higher fine amounts had an immediate and significant monetary impact.", "#22c55e"),
          finding_item("Compliance Effect",   "Post-Act compliance changes indicate short-term deterrence. The COVID exclusion column isolates the true sustained behavioural effect.", "#fbbf24"),
          finding_item("Court Burden Grew",   "Statistically significant increase in court burden confirms higher fines gave offenders stronger financial motivation to legally challenge challans.", "#f97316"),
          finding_item("COVID Isolation",     "Comparing \u2018After\u2019 with \u2018After (Excl. COVID)\u2019 separates the true policy effect from the 2020\u20132021 lockdown-driven enforcement disruption.", "#c084fc")
        )
      ),

      # ════════════════════════════════════════════════════
      # TAB 5 — FORECASTING
      # ════════════════════════════════════════════════════
      tabItem(tabName = "forecast",
        tags$div(class="page-header",
          tags$div(class="accent-bar"),
          tags$h2("Time Series Forecasting"),
          tags$div(class="page-sub","ARIMA and ETS models forecasting future challans and revenue up to 24 months ahead")
        ),
        desc_box("chart-area","About This Analysis",
          "Two complementary time-series models are used. ARIMA (Auto-Regressive Integrated Moving Average) captures trends
           and autocorrelation patterns in the data. ETS (Error, Trend, Seasonality) assigns greater weight to recent
           observations and handles seasonal patterns well. Both produce a point forecast (best guess) along with
           95% confidence intervals showing the range where future values are likely to fall.
           Accuracy is evaluated using RMSE and MAPE \u2014 lower values indicate better model fit."
        ),
        fluidRow(
          box(width=3,solidHeader=TRUE,status="primary",
              title=tagList(icon("sliders-h")," Forecast Settings"),
              selectInput("fc_target","Variable to Forecast:",
                choices=c("Total Challans Issued"="totalChallan",
                          "Total Revenue (Rs)"="totalAmount",
                          "Pending Challans"="pendingChallan",
                          "Pending Revenue (Rs)"="pendingAmount")),
              sliderInput("fc_h","Months to Forecast Ahead:",min=3,max=24,value=12,step=1),
              selectInput("fc_show","Models to Display:",
                choices=c("Both ARIMA & ETS"="both",
                          "ARIMA only"="arima",
                          "ETS only"="ets")),
              br(),
              actionButton("run_fc","Run Forecast",class="btn-run",icon=icon("play")),
              tags$div(class="acc-panel",
                tags$div(class="acc-title",icon("bullseye")," Model Accuracy"),
                verbatimTextOutput("fc_accuracy")
              )
          ),
          box(width=9,solidHeader=TRUE,status="primary",
              title=tagList(icon("chart-area")," Forecast Chart"),
              tags$p("Grey = historical.  Blue dashed = ARIMA forecast.  Red dotted = ETS forecast.  Shaded bands = 95% confidence interval.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("fc_plot",height=415))
        ),
        fluidRow(
          box(width=12,solidHeader=TRUE,status="info",
              title=tagList(icon("table")," Forecasted Values "),
              tags$div(class="fc-legend",
                tags$div(class="fc-legend-item",tags$div(class="fc-dot",style="background:#3b82f6;"),
                         tags$span(tags$b("ARIMA Best Guess")," \u2014 ARIMA model\u2019s single most likely prediction")),
                tags$div(class="fc-legend-item",tags$div(class="fc-dot",style="background:#1e3a5f;"),
                         tags$span(tags$b("ARIMA Lowest / Highest")," \u2014 the range it could realistically fall in (95% confidence)")),
                tags$div(class="fc-legend-item",tags$div(class="fc-dot",style="background:#ef4444;"),
                         tags$span(tags$b("ETS Best Guess")," \u2014 ETS model\u2019s single most likely prediction")),
                tags$div(class="fc-legend-item",tags$div(class="fc-dot",style="background:#431407;"),
                         tags$span(tags$b("ETS Lowest / Highest")," \u2014 the range it could realistically fall in (95% confidence)")),
                tags$div(class="fc-legend-item",tags$div(class="fc-dot",style="background:#2d4a60;"),
                         tags$span("A Lowest value of 0 means the model is uncertain about the lower bound that far ahead \u2014 this is normal and expected"))
              ),
              DTOutput("fc_table"))
        ),
        uiOutput("fc_findings")
      ),

      # ════════════════════════════════════════════════════
      # TAB 6 — CORRELATION
      # ════════════════════════════════════════════════════
      tabItem(tabName = "correlation",
        tags$div(class="page-header",
          tags$div(class="accent-bar"),
          tags$h2("Correlation Analysis"),
          tags$div(class="page-sub","Statistical relationships between all key metrics of the e-challan system")
        ),
        desc_box("project-diagram","About This Analysis",
          "Pearson correlation coefficients measure the linear relationship between pairs of metrics on a scale from -1 to +1.
           A value near +1 means both metrics move together (e.g. more challans issued \u2192 more revenue).
           A value near -1 means they move in opposite directions. Values near 0 indicate no linear relationship.
           The scatter plot explorer lets you investigate any two-variable relationship directly."
        ),
        fluidRow(
          box(width=7,solidHeader=TRUE,status="primary",
              title=tagList(icon("th")," Correlation Heatmap"),
              tags$p("Dark blue = strong positive correlation.  Dark red = strong negative.  Hover for exact r value.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              plotlyOutput("cor_heatmap",height=430)),
          box(width=5,solidHeader=TRUE,status="info",
              title=tagList(icon("sort-amount-down")," Correlation Strength vs Total Challans"),
              tags$p("Ranked by how strongly each metric correlates with Total Challans issued.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              DTOutput("cor_table"))
        ),
        fluidRow(
          box(width=6,solidHeader=TRUE,status="success",
              title=tagList(icon("dot-circle")," Scatter Plot Explorer"),
              tags$p("Select any two metrics. Red line = linear trend. r = correlation coefficient.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              fluidRow(
                column(6,selectInput("sc_x","X-Axis Metric:",
                  choices=c("Total Challans"="totalChallan","Total Revenue"="totalAmount",
                            "Compliance %"="compliance_rate","Backlog %"="backlog_rate",
                            "Collection %"="collection_rate","Pending Challans"="pendingChallan",
                            "Pending Revenue"="pendingAmount","Court Cases"="totalCourt"))),
                column(6,selectInput("sc_y","Y-Axis Metric:",
                  choices=c("Total Revenue"="totalAmount","Total Challans"="totalChallan",
                            "Compliance %"="compliance_rate","Backlog %"="backlog_rate",
                            "Collection %"="collection_rate","Pending Challans"="pendingChallan",
                            "Pending Revenue"="pendingAmount","Court Cases"="totalCourt")))
              ),
              plotlyOutput("sc_plot",height=295)),
          box(width=6,solidHeader=TRUE,status="warning",
              title=tagList(icon("calculator")," Descriptive Statistics (Monthly)"),
              tags$p("Summary statistics for all key metrics aggregated at the monthly level across 2015\u20132025.",style="color:#2d4a60;font-size:11.5px;margin:0 0 10px;"),
              DTOutput("stat_table"))
        ),
        findings_box(
          finding_item("Challans \u2194 Revenue",     "Total Challans and Total Revenue show a very strong positive correlation \u2014 more enforcement directly drives higher revenue collection.", "#38bdf8"),
          finding_item("Backlog \u2194 Court Burden", "Backlog rate and court burden rate are positively correlated, confirming systems with more unresolved challans also face greater judicial pressure.", "#f97316"),
          finding_item("Compliance \u2194 Backlog",   "Compliance rate and backlog rate are negatively correlated \u2014 as more challans get paid, fewer remain unresolved, reducing backlog.", "#f87171"),
          finding_item("Revenue per Challan",         "Revenue per challan shows a moderate positive correlation with compliance rate \u2014 higher-value fines may incentivise better behaviour but also more court contestation.", "#c084fc")
        )
      )
    )
  )
)

# ════════════════════════════════════════════════════════════
# SERVER
# ════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── TREND ──────────────────────────────────────────────────
  output$tr_challans <- renderPlotly({
    plot_ly(yearly, x=~year) %>%
      add_bars(y=~totalChallan/1e6, name="Challans (M)",
               marker=list(color="rgba(59,130,246,0.7)",line=list(color="#3b82f6",width=1.2)),
               hovertemplate="%{x}: %{y:.2f}M challans<extra></extra>") %>%
      add_lines(y=~totalChallan/1e6, showlegend=FALSE,
                line=list(color="#7dd3fc",width=2.5)) %>%
      dk(title_x="Year", title_y="Challans (Millions)") %>%
      layout(showlegend=FALSE)
  })

  output$tr_revenue <- renderPlotly({
    plot_ly(yearly, x=~year) %>%
      add_bars(y=~totalAmount/1e7, name="Revenue",
               marker=list(color="rgba(34,197,94,0.7)",line=list(color="#22c55e",width=1.2)),
               hovertemplate="%{x}: Rs%{y:.1f} Cr<extra></extra>") %>%
      add_lines(y=~totalAmount/1e7, showlegend=FALSE,
                line=list(color="#86efac",width=2.5)) %>%
      dk(title_x="Year", title_y="Revenue (Rs Crore)") %>%
      layout(showlegend=FALSE)
  })

  output$tr_rates <- renderPlotly({
    plot_ly(yearly, x=~year) %>%
      add_lines(y=~compliance_rate, name="Compliance Rate %",
                line=list(color="#22c55e",width=2.5),
                hovertemplate="%{x}: %{y:.1f}%<extra>Compliance</extra>") %>%
      add_lines(y=~court_burden_rate, name="Court Burden Rate %",
                line=list(color="#f97316",width=2.5,dash="dash"),
                hovertemplate="%{x}: %{y:.1f}%<extra>Court Burden</extra>") %>%
      dk(title_x="Year", title_y="Rate (%)") %>%
      layout(hovermode="x unified")
  })

  output$tr_growth <- renderPlotly({
    d <- yearly[!is.na(yearly$growth_challan),]
    plot_ly(d, x=~year) %>%
      add_bars(y=~growth_challan, name="Challan Growth %",
               marker=list(color=ifelse(d$growth_challan>=0,"rgba(59,130,246,0.8)","rgba(239,68,68,0.8)")),
               hovertemplate="%{x} Challans: %{y:.1f}%<extra></extra>") %>%
      add_bars(y=~growth_revenue, name="Revenue Growth %",
               marker=list(color=ifelse(d$growth_revenue>=0,"rgba(34,197,94,0.8)","rgba(249,115,22,0.8)")),
               hovertemplate="%{x} Revenue: %{y:.1f}%<extra></extra>") %>%
      add_segments(x=min(d$year)-0.5, xend=max(d$year)+0.5, y=0, yend=0,
                   showlegend=FALSE, line=list(color="#2d4a60",dash="dash",width=1)) %>%
      dk(title_x="Year", title_y="Growth (%)") %>%
      layout(barmode="group")
  })

  # ── REVENUE ────────────────────────────────────────────────
  output$rv_per <- renderPlotly({
    plot_ly(monthly, x=~date, y=~revenue_per_challan, type="scatter", mode="lines",
            line=list(color="#c084fc",width=1.5),
            hovertemplate="%{x|%b %Y}: Rs%{y:.0f}<extra></extra>") %>%
      dk(title_x="Month", title_y="Rs per Challan")
  })

  output$rv_gap <- renderPlotly({
    plot_ly(monthly, x=~date, y=~pendingAmount/1e7, type="scatter", mode="lines",
            fill="tozeroy", fillcolor="rgba(239,68,68,0.15)",
            line=list(color="#ef4444",width=1.5),
            hovertemplate="%{x|%b %Y}: Rs%{y:.1f} Cr<extra></extra>") %>%
      dk(title_x="Month", title_y="Uncollected (Rs Crore)")
  })

  output$rv_rate <- renderPlotly({
    avg <- mean(monthly$collection_rate, na.rm=TRUE)
    plot_ly(monthly, x=~date, y=~collection_rate, type="scatter", mode="lines",
            line=list(color="#34d399",width=1.5),
            hovertemplate="%{x|%b %Y}: %{y:.1f}%<extra></extra>") %>%
      add_segments(x=min(monthly$date), xend=max(monthly$date),
                   y=avg, yend=avg, name=paste0("Avg: ",round(avg,1),"%"),
                   line=list(color="#f87171",dash="dash",width=1.5)) %>%
      dk(title_x="Month", title_y="Collection Rate (%)")
  })

  output$rv_stack <- renderPlotly({
    plot_ly(monthly, x=~date) %>%
      add_bars(y=~disposedAmount/1e7, name="Collected", marker=list(color="rgba(34,197,94,0.8)")) %>%
      add_bars(y=~pendingAmount/1e7,  name="Pending",   marker=list(color="rgba(239,68,68,0.8)")) %>%
      dk(title_x="Month", title_y="Amount (Rs Crore)") %>%
      layout(barmode="stack")
  })

  # ── BACKLOG ────────────────────────────────────────────────
  output$bl_trend <- renderPlotly({
    plot_ly(monthly, x=~date) %>%
      add_lines(y=~disposedChallan/1e3, name="Disposed (Resolved)",  line=list(color="#34d399",width=2)) %>%
      add_lines(y=~pendingChallan/1e3,  name="Pending (Unresolved)", line=list(color="#f87171",width=2)) %>%
      dk(title_x="Month", title_y="Challans (Thousands)") %>%
      layout(hovermode="x unified")
  })

  output$bl_court <- renderPlotly({
    avg <- mean(monthly$court_burden_rate, na.rm=TRUE)
    plot_ly(monthly, x=~date, y=~court_burden_rate, type="scatter", mode="lines",
            fill="tozeroy", fillcolor="rgba(249,115,22,0.12)",
            line=list(color="#f97316",width=1.5),
            hovertemplate="%{x|%b %Y}: %{y:.1f}%<extra></extra>") %>%
      add_segments(x=min(monthly$date), xend=max(monthly$date),
                   y=avg, yend=avg, name=paste0("Avg: ",round(avg,1),"%"),
                   line=list(color="#f87171",dash="dash",width=1.5)) %>%
      dk(title_x="Month", title_y="Court Burden Rate (%)")
  })

  output$bl_table <- renderDT({
    tbl <- get_yearly_backlog_table(yearly)
    colnames(tbl) <- c("Year","Total Challans","Disposed","Pending",
                       "Revenue (Cr)","Collected (Cr)","Pending (Cr)",
                       "Compliance %","Backlog %","Court Burden %")
    datatable(tbl, rownames=FALSE,
              options=list(scrollX=TRUE, pageLength=15, dom="t")) %>%
      formatStyle(columns=1:10, backgroundColor=BOX, color="#8daabf") %>%
      formatStyle("Compliance %",   fontWeight="bold", color="#34d399") %>%
      formatStyle("Backlog %",      fontWeight="bold", color="#f87171") %>%
      formatStyle("Court Burden %", fontWeight="bold", color="#f97316")
  })

  # ── POLICY ─────────────────────────────────────────────────
  pol <- reactive({ run_policy_analysis(monthly) })

  prow <- function(metric_label) {
    s <- pol()$summary
    s[s$Metric == metric_label, ]
  }

  output$pol_cb_before <- renderUI(
    kpi_card(formatC(round(prow("Total Challans")$Before), format="d", big.mark=","),
             "Avg Monthly Challans BEFORE", "car", ACC))
  output$pol_cb_after <- renderUI(
    kpi_card(formatC(round(prow("Total Challans")$After), format="d", big.mark=","),
             "Avg Monthly Challans AFTER", "car", "#6366f1"))
  output$pol_cb_change <- renderUI({
    r  <- prow("Total Challans"); pc <- r$Pct_Change
    col <- if (!is.na(pc) && pc >= 0) "#34d399" else "#f87171"
    kpi_card(paste0(ifelse(!is.na(pc)&pc>=0,"+",""), round(pc,1), "%"),
             "Change in Challans",
             ifelse(!is.na(pc)&pc>=0,"arrow-up","arrow-down"), col)
  })
  output$pol_cb_sig <- renderUI({
    r <- prow("Total Challans")
    col <- if (r$Significant=="Yes") "#22c55e" else "#f97316"
    kpi_card(paste0("p = ", r$P_Value),
             ifelse(r$Significant=="Yes","Statistically Significant","Not Significant"),
             "flask", col)
  })

  output$pol_rv_before <- renderUI(
    kpi_card(paste0("Rs ", round(prow("Total Revenue (Rs)")$Before/1e7,1)," Cr"),
             "Avg Monthly Revenue BEFORE", "rupee-sign", "#22c55e"))
  output$pol_rv_after <- renderUI(
    kpi_card(paste0("Rs ", round(prow("Total Revenue (Rs)")$After/1e7,1)," Cr"),
             "Avg Monthly Revenue AFTER", "rupee-sign", "#86efac"))
  output$pol_rv_change <- renderUI({
    r <- prow("Total Revenue (Rs)"); pc <- r$Pct_Change
    col <- if (!is.na(pc) && pc >= 0) "#34d399" else "#f87171"
    kpi_card(paste0(ifelse(!is.na(pc)&pc>=0,"+",""), round(pc,1), "%"),
             "Change in Revenue",
             ifelse(!is.na(pc)&pc>=0,"arrow-up","arrow-down"), col)
  })
  output$pol_rv_sig <- renderUI({
    r <- prow("Total Revenue (Rs)")
    col <- if (r$Significant=="Yes") "#22c55e" else "#f97316"
    kpi_card(paste0("p = ", r$P_Value),
             ifelse(r$Significant=="Yes","Statistically Significant","Not Significant"),
             "flask", col)
  })

  make_tl <- function(col, ytitle, c_before, c_after) {
    d   <- pol()$monthly; cut <- pol()$cutoff
    cs  <- pol()$covid_start; ce <- pol()$covid_end
    is_m <- col %in% c("totalAmount","pendingAmount")
    div  <- ifelse(is_m, 1e7, 1e3)
    ymax <- max(d[[col]], na.rm=TRUE)/div * 1.1
    b_d <- d[d$date<cut,]; a_d <- d[d$date>=cut,]
    plot_ly() %>%
      add_ribbons(x=c(cs,ce,ce,cs), ymin=0, ymax=ymax, name="COVID-19 Period",
                  fillcolor="rgba(253,224,71,0.08)", line=list(color="transparent")) %>%
      add_lines(data=b_d, x=~date, y=~get(col)/div, name="Before MV Act",
                line=list(color=c_before,width=2)) %>%
      add_lines(data=a_d, x=~date, y=~get(col)/div, name="After MV Act",
                line=list(color=c_after,width=2)) %>%
      add_segments(x=cut, xend=cut, y=0, yend=ymax, showlegend=FALSE,
                   line=list(color="#f87171",dash="dash",width=2)) %>%
      dk(title_x="Month",
         title_y=paste(ytitle, ifelse(is_m,"(Rs Crore)","(Thousands)"))) %>%
      layout(hovermode="x unified",
             annotations=list(list(x=cut, y=ymax*0.88, text="Oct 2019",
               showarrow=TRUE, arrowhead=2, arrowcolor="#f87171",
               font=list(color="#f87171",size=11))))
  }

  output$pol_challan    <- renderPlotly({ make_tl("totalChallan","Challans","#7dd3fc","#3b82f6") })
  output$pol_revenue    <- renderPlotly({ make_tl("totalAmount","Revenue","#86efac","#22c55e") })

  output$pol_compliance <- renderPlotly({
    d <- pol()$monthly; cut <- pol()$cutoff
    cs <- pol()$covid_start; ce <- pol()$covid_end
    b_d <- d[d$date<cut,]; a_d <- d[d$date>=cut,]
    plot_ly() %>%
      add_ribbons(x=c(cs,ce,ce,cs), ymin=0, ymax=100, name="COVID-19",
                  fillcolor="rgba(253,224,71,0.08)", line=list(color="transparent")) %>%
      add_lines(data=b_d, x=~date, y=~compliance_rate, name="Before",
                line=list(color="#fbbf24",width=2)) %>%
      add_lines(data=a_d, x=~date, y=~compliance_rate, name="After",
                line=list(color="#34d399",width=2)) %>%
      add_segments(x=cut, xend=cut, y=0, yend=100, showlegend=FALSE,
                   line=list(color="#f87171",dash="dash",width=2)) %>%
      dk(title_x="Month", title_y="Compliance Rate (%)") %>%
      layout(hovermode="x unified")
  })

  output$pol_backlog <- renderPlotly({
    d <- pol()$monthly; cut <- pol()$cutoff
    cs <- pol()$covid_start; ce <- pol()$covid_end
    b_d <- d[d$date<cut,]; a_d <- d[d$date>=cut,]
    plot_ly() %>%
      add_ribbons(x=c(cs,ce,ce,cs), ymin=0, ymax=100, name="COVID-19",
                  fillcolor="rgba(253,224,71,0.08)", line=list(color="transparent")) %>%
      add_lines(data=b_d, x=~date, y=~backlog_rate,       name="Backlog Before",      line=list(color="#f87171",width=2)) %>%
      add_lines(data=a_d, x=~date, y=~backlog_rate,       name="Backlog After",       line=list(color="#ef4444",width=2)) %>%
      add_lines(data=b_d, x=~date, y=~court_burden_rate,  name="Court Burden Before", line=list(color="#c084fc",width=2,dash="dot")) %>%
      add_lines(data=a_d, x=~date, y=~court_burden_rate,  name="Court Burden After",  line=list(color="#a855f7",width=2,dash="dot")) %>%
      add_segments(x=cut, xend=cut, y=0, yend=100, showlegend=FALSE,
                   line=list(color="#f87171",dash="dash",width=2)) %>%
      dk(title_x="Month", title_y="Rate (%)") %>%
      layout(hovermode="x unified")
  })

  output$pol_table <- renderDT({
    s <- pol()$summary
    colnames(s) <- c("Metric","Before Oct 2019","After Oct 2019",
                     "After (Excl. COVID)","% Change","P-Value","Significant?")
    datatable(s, rownames=FALSE,
              options=list(scrollX=TRUE, dom="t")) %>%
      formatStyle(columns=1:7, backgroundColor=BOX, color="#8daabf") %>%
      formatStyle("% Change",
                  color=styleInterval(0, c("#f87171","#34d399")), fontWeight="bold") %>%
      formatStyle("Significant?",
                  backgroundColor=styleEqual(c("Yes","No"),c("#14532d","#431407")),
                  color=styleEqual(c("Yes","No"),c("#34d399","#f87171")),
                  fontWeight="bold")
  })

  # ── FORECASTING ────────────────────────────────────────────
  fc_res <- eventReactive(input$run_fc, {
    withProgress(message="Fitting forecast models...", value=0.5,
      run_forecast(monthly, col=input$fc_target, h=input$fc_h))
  }, ignoreNULL=FALSE)

  output$fc_plot <- renderPlotly({
    req(fc_res()); fc <- fc_res()
    is_m <- input$fc_target %in% c("totalAmount","pendingAmount")
    div  <- ifelse(is_m, 1e7, 1)
    ylab <- ifelse(is_m, "Amount (Rs Crore)", "Count")
    ts_v <- as.numeric(fc$ts)
    ts_t <- seq(monthly$date[1], by="month", length.out=length(ts_v))
    fc_t <- seq(fc$last_date %m+% months(1), by="month", length.out=fc$h)
    p <- plot_ly() %>%
      add_lines(x=ts_t, y=ts_v/div, name="Historical",
                line=list(color="#475569",width=1.5),
                hovertemplate="%{x|%b %Y}: %{y:.2f}<extra>Historical</extra>")
    if (input$fc_show %in% c("both","arima")) {
      p <- p %>%
        add_ribbons(x=fc_t, ymin=pmax(as.numeric(fc$arima$lower[,2])/div,0),
                    ymax=as.numeric(fc$arima$upper[,2])/div, name="ARIMA 95% CI",
                    fillcolor="rgba(59,130,246,0.15)", line=list(color="transparent")) %>%
        add_lines(x=fc_t, y=as.numeric(fc$arima$mean)/div, name="ARIMA Forecast",
                  line=list(color="#3b82f6",dash="dash",width=2.5),
                  hovertemplate="%{x|%b %Y}: %{y:.2f}<extra>ARIMA</extra>")
    }
    if (input$fc_show %in% c("both","ets")) {
      p <- p %>%
        add_ribbons(x=fc_t, ymin=pmax(as.numeric(fc$ets$lower[,2])/div,0),
                    ymax=as.numeric(fc$ets$upper[,2])/div, name="ETS 95% CI",
                    fillcolor="rgba(239,68,68,0.12)", line=list(color="transparent")) %>%
        add_lines(x=fc_t, y=as.numeric(fc$ets$mean)/div, name="ETS Forecast",
                  line=list(color="#ef4444",dash="dot",width=2.5),
                  hovertemplate="%{x|%b %Y}: %{y:.2f}<extra>ETS</extra>")
    }
    p %>% dk(title_x="Date", title_y=ylab) %>%
      layout(hovermode="x unified")
  })

  output$fc_accuracy <- renderText({
    req(fc_res())
    get_accuracy_text(fc_res(), monthly, input$fc_target)
  })

  output$fc_table <- renderDT({
    req(fc_res())
    fc   <- fc_res()
    is_m <- input$fc_target %in% c("totalAmount","pendingAmount")
    tbl  <- build_forecast_table(fc)

    # Format all numeric columns in human-readable form
    for (j in 2:7) {
      tbl[[j]] <- sapply(tbl[[j]], function(x) {
        if (is_m) {
          if (x == 0) return("0")
          paste0("Rs ", round(x / 1e7, 2), " Cr")
        } else {
          fmt_lakh(x)
        }
      })
    }

    # Plain English column headers — NO mismatch with formatStyle
    # We apply formatStyle BEFORE renaming by using column index
    dt <- datatable(tbl, rownames=FALSE,
              colnames=c("Month",
                         "ARIMA Best Guess","ARIMA Lowest","ARIMA Highest",
                         "ETS Best Guess",  "ETS Lowest",  "ETS Highest"),
              options=list(pageLength=12, scrollX=TRUE, dom="t",
                           columnDefs=list(list(className="dt-center", targets=1:6)))) %>%
      formatStyle(columns=1:7,  backgroundColor=BOX,   color="#8daabf") %>%
      formatStyle(columns=2,    fontWeight="bold",      color="#7dd3fc", backgroundColor="#0d1e30") %>%
      formatStyle(columns=5,    fontWeight="bold",      color="#fca5a5", backgroundColor="#1a0f0f") %>%
      formatStyle(columns=c(3,4,6,7), color="#3d5a70")
    dt
  })

  # Dynamic findings box for forecast tab
  output$fc_findings <- renderUI({
    req(fc_res()); fc <- fc_res()
    is_m <- input$fc_target %in% c("totalAmount","pendingAmount")
    arima_vals <- as.numeric(fc$arima$mean)
    ets_vals   <- as.numeric(fc$ets$mean)
    last_hist  <- as.numeric(tail(fc$ts, 1))
    arima_chg  <- round((arima_vals[1] - last_hist) / last_hist * 100, 1)
    agreement  <- abs(mean(arima_vals) - mean(ets_vals)) / ((mean(arima_vals) + mean(ets_vals))/2) * 100
    arima_fmt  <- if(is_m) paste0("Rs ", round(arima_vals[1]/1e7,1)," Cr") else fmt_lakh(arima_vals[1])
    ets_fmt    <- if(is_m) paste0("Rs ", round(ets_vals[1]/1e7,1)," Cr")   else fmt_lakh(ets_vals[1])
    agree_txt  <- if(agreement < 10) "high agreement \u2014 prediction is reliable"
                  else if(agreement < 25) "moderate agreement \u2014 use with some caution"
                  else "low agreement \u2014 models disagree, interpret carefully"

    findings_box(
      finding_item("Next Month \u2014 ARIMA",
        paste0("ARIMA predicts ", arima_fmt, " for the first forecast month (",
               ifelse(arima_chg>=0,"+",""), arima_chg, "% vs last historical month)."), "#3b82f6"),
      finding_item("Next Month \u2014 ETS",
        paste0("ETS predicts ", ets_fmt, " for the same month. ETS weights recent data more, so it may better capture current trends."), "#ef4444"),
      finding_item("Model Agreement",
        paste0("The two models show ", agree_txt, ". When both models predict similar values, confidence is higher."), "#22c55e"),
      finding_item("Widening Bands",
        "The shaded confidence bands on the chart widen over time \u2014 this is correct. Uncertainty naturally grows the further ahead you forecast.", "#fbbf24"),
    )
  })

  # ── CORRELATION ────────────────────────────────────────────
  cor_mat <- reactive({ run_correlation(monthly) })

  output$cor_heatmap <- renderPlotly({
    mat <- cor_mat()
    plot_ly(x=colnames(mat), y=rownames(mat), z=mat,
            type="heatmap",
            colorscale=list(list(0,"#7f1d1d"),list(0.5,BOX),list(1,"#1d4ed8")),
            zmin=-1, zmax=1,
            hovertemplate="%{y} vs %{x}<br>r = %{z:.3f}<extra></extra>") %>%
      dk() %>%
      layout(xaxis=list(tickangle=-45,tickfont=list(size=10,color=FONT),title=""),
             yaxis=list(tickfont=list(size=10,color=FONT),title=""),
             margin=list(b=150,l=150,t=10,r=10))
  })

  output$cor_table <- renderDT({
    tbl <- get_correlation_strength_table(cor_mat())
    datatable(tbl, rownames=FALSE,
              options=list(pageLength=12, dom="t")) %>%
      formatStyle(columns=1:4, backgroundColor=BOX, color="#8daabf") %>%
      formatStyle("Strength",
        color=styleEqual(c("Very Strong","Strong","Moderate","Weak","Negligible"),
                         c("#fff","#fff","#fff","#8daabf","#3d5a70")),
        backgroundColor=styleEqual(c("Very Strong","Strong","Moderate","Weak","Negligible"),
                                   c("#1d4ed8","#2563eb","#3b82f6","#1e3a5f",BOX)),
        fontWeight="bold") %>%
      formatStyle("Direction",
        color=styleEqual(c("Positive \u2191","Negative \u2193"),c("#34d399","#f87171")),
        fontWeight="bold")
  })

  output$sc_plot <- renderPlotly({
    xcol <- input$sc_x; ycol <- input$sc_y
    xv <- monthly[[xcol]]; yv <- monthly[[ycol]]
    ok <- !is.na(xv) & !is.na(yv)
    cr <- round(cor(xv[ok], yv[ok]), 3)
    str <- ifelse(abs(cr)>=0.6,"Strong",ifelse(abs(cr)>=0.4,"Moderate","Weak"))
    fit <- lm(yv[ok] ~ xv[ok])
    xs  <- seq(min(xv[ok]), max(xv[ok]), length.out=100)
    ys  <- coef(fit)[1] + coef(fit)[2] * xs
    plot_ly() %>%
      add_markers(x=xv[ok], y=yv[ok], showlegend=FALSE,
                  marker=list(color="rgba(56,189,248,0.4)",size=6),
                  hovertemplate=paste0(xcol,": %{x:.1f}<br>",ycol,": %{y:.1f}<extra></extra>")) %>%
      add_lines(x=xs, y=ys, name=paste0("r = ",cr,"  (",str,")"),
                line=list(color="#f87171",width=2)) %>%
      dk(title_x=xcol, title_y=ycol)
  })

  output$stat_table <- renderDT({
    tbl <- get_descriptive_stats(monthly)
    datatable(tbl, rownames=FALSE,
              options=list(pageLength=12, dom="t", scrollX=TRUE)) %>%
      formatStyle(columns=1:6, backgroundColor=BOX, color="#8daabf") %>%
      formatStyle("Metric", fontWeight="bold", color=ACC)
  })
}

shinyApp(ui, server)
