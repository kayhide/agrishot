module.exports = {
  received_text: "被害にあった農産品を撮影してアップロードしてください。",
  received_image: "画像を受け付けました。",
  might_be_wrong: "予測は人工知能により自動で行っていますので間違いの可能性もあります。",
  contact_here: (id) => "専門スタッフへの問い合わせはこちら。\nline://home/public/main?id=" + id,
  predictions: (items) => ["予測:", ...items].join("\n")
}
